=head1 NAME

SQL::SyntaxModel - An abstract syntax tree for all types of SQL

=cut

######################################################################

package SQL::SyntaxModel;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.40';

use Locale::KeyedText 0.06;

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Locale::KeyedText 0.06 (for error messages)

=head1 COPYRIGHT AND LICENSE

This file is part of the SQL::SyntaxModel library (libSQLSM).

SQL::SyntaxModel is Copyright (c) 1999-2004, Darren R. Duncan.  All rights reserved.
Address comments, suggestions, and bug reports to B<perl@DarrenDuncan.net>, or
visit "http://www.DarrenDuncan.net" for more information.

SQL::SyntaxModel is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License (GPL) version 2 as published
by the Free Software Foundation (http://www.fsf.org/).  You should have
received a copy of the GPL as part of the SQL::SyntaxModel distribution, in the
file named "LICENSE"; if not, write to the Free Software Foundation, Inc., 59
Temple Place, Suite 330, Boston, MA 02111-1307 USA.

Linking SQL::SyntaxModel statically or dynamically with other modules is making
a combined work based on SQL::SyntaxModel.  Thus, the terms and conditions of
the GPL cover the whole combination.  As a special exception, the copyright
holders of SQL::SyntaxModel give you permission to link SQL::SyntaxModel with
independent modules, regardless of the license terms of these independent
modules, and to copy and distribute the resulting combined work under terms of
your choice, provided that every copy of the combined work is accompanied by a
complete copy of the source code of SQL::SyntaxModel (the version of
SQL::SyntaxModel used to produce the combined work), being distributed under
the terms of the GPL plus this exception.  An independent module is a module
which is not derived from or based on SQL::SyntaxModel, and which is fully
useable when not linked to SQL::SyntaxModel in any form.

Any versions of SQL::SyntaxModel that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. SQL::SyntaxModel is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of SQL::SyntaxModel
would appreciate being informed any time you create a modified version of
SQL::SyntaxModel that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=cut

######################################################################
######################################################################

# Names of properties for objects of the SQL::SyntaxModel::Container class are declared here:
my $CPROP_ALL_NODES = 'all_nodes'; # hash of hashes of Node refs; find any Node by node_type:node_id quickly
my $CPROP_PSEUDONODES = 'pseudonodes'; # hash of arrays of Node refs
	# This property is for remembering the insert order of Nodes having hardwired pseudonode parents
my $CPROP_NEXT_FREE_NIDS = 'next_free_nids'; # hash (enum,id); next free node ids per node type
	# Each property key is a valid node type, and the associated value is an integer that 
	# is one higher than the highest Node ID that is or was in use by a Node in this Container.
my $CPROP_DEF_CON_TESTED = 'def_con_tested'; # boolean - true by def, false when changes made
	# This property is a status flag which says there have been no changes to the Nodes 
	# in this Container since the last time test_deferrable_constraints() passed its tests, 
	# and so the current Nodes are still valid.  It is used internally by 
	# test_deferrable_constraints() to make code faster by avoiding un-necessary 
	# repeated tests from multiple external Container.test_deferrable_constraints() calls.
	# It is set true on a new empty Container, and set false when any Nodes are moved in 
	# or out of the "well known" state within that Container, or are changed while in that state.
#my $CPROP_CURR_NODE = 'curr_node'; # ref to a Node; used when "streaming" to or from XML
	# I may instead make a new inner class for this, and there can be several of these 
	# per container, such as if multiple streams are working in different areas at once; 
	# any Container property would then just have a list of those active objects, 
	# so they can be killed (return links to Container obj broken) if their Container is destroyed.
# To do: have attribute to indicate an edit in progress 
	# or that there was a failure resulting in inconsistant data;
	# this may be set by a method which partly implements a data change 
	# which is not backed out of, before that function throws an exception;
	# this property may best just be inside the thrown Locale::KeyedText object;
	# OTOH, if users have coarse-grained locks on Containers for threads, we could have a property,
	# since a call to an editing method would check and clear that before the thread releases lock

# Names of properties for objects of the SQL::SyntaxModel::Node class are declared here:
	# The C version will have the following comprise fields in a Node struct;
	# all fields will be integers or memory references or enums; none will be strings.
my $NPROP_NODE_TYPE   = 'node_type'; # str (enum) - what type of Node this is, can not change once set
	# The Node type is the only property which absolutely can not change, and is set when object created.
	# (All other Node properties start out undefined or false, and are set separately from object creation.)
	# C version of this will be an enumerated value.
my $NPROP_NODE_ID     = 'node_id'; # uint - unique identifier attribute for this node within container+type
	# Node id must be set when/before Node is put in a container; may lack one when not in container.
	# C version of this will be an unsigned integer.
my $NPROP_AT_LITERALS = 'at_literals'; # hash (enum,lit) - attrs of Node which are non-enum, non-id literal values
	# C version of this will be an array (pointer) of Literal structs.
	# We already know what all the attributes can be for each node type, so the size of the array 
	# will be fixed and known in advance, allowing it to be all allocated with one malloc() call.
	# Each attribute struct would be at a specific array index; 
	# C macros/constants will give names to the indices, like with the hash keys for the above.
my $NPROP_AT_ENUMS    = 'at_enums'; # hash (enum,enum) - attrs of Node which are enumerated values
	# C version of this will be an array (pointer) of enumerated values.
my $NPROP_AT_NREFS    = 'at_nrefs'; # hash (enum,Node) - attrs of Node which point to other Nodes (or ids rep other Nodes)
	# C version of this will be either multiple arrays or a single array of structs, to handle pointer vs uint
	# Hash elements can only be actual references when Node is in a Container, and pointed to must be in same
	# When converting to XML, if P_NODE_ATNM is set, the AT_NREF it refers to won't become an XML attr (redundant)
my $NPROP_P_NODE_ATNM = 'p_node_atnm'; # str (enum) - name of AT_NREFS elem having our primary parent Node, if any
	# When this property is valued, there is no implication that the corres AT_NREFS is also valued
	# C version of this will be an enumerated value.
	# Since a Node of one type may have a parent Node of multiple possible types, 
	# this tells us not only which type but which instance it is.
	# This property will be undefined if either there is no parent or the parent is a pseudo-node.
my $NPROP_CONTAINER   = 'container'; # ref to Container this Node lives in
	# C version of this would be a pointer to a Container struct
my $NPROP_LINKS_RECIP = 'links_recip'; # boolean - false by def, true when our actual refs in AT_NREFS are reciprocated
	# C version of this will be an integer used like a boolean.
my $NPROP_CHILD_NODES = 'child_nodes'; # array - list of refs to other Nodes having actual refs to this one
	# We use this to reciprocate actual refs from the AT_NREFS property of other Nodes to us.
	# When converting to XML, we only render once, beneath the Node which we refer to in our P_NODE_ATNM.
	# C version will be a double-linked list with each element representing a Node struct.
	# It is important to ensure that if a Node links to us multiple times (via multiple AT_NREFS) 
	# then we include the other Node in our child list just as many times; eg: 2 here means 2 back; 
	# however, when rendering to XML, we only render a Node once, and not as many times as linked; 
	# it is also possible that we may never be put in this situation from real-world usage.
	# Note that in the above situation, a normalized child list would have the above two links sitting 
	# adjacent to each other; add_reciprocal_links() will do this, but subsequent calls to 
	# set_node_ref_attribute() might not.  In the interest of simplicity, any method that wants to 
	# change the order of a child list should also normalize any multiple same-child occurrances.

# These are programmatically recognized enumerations of values that 
# particular Node attributes are allowed to have.  They are given names 
# here so that multiple Node types can make use of the same value lists.  
# Currently only the codes are shown, but attributes may be attached later.
my %ENUMERATED_TYPES = (
	'privilege_type' => { map { ($_ => 1) } qw(
		ALL SELECT DELETE INSERT UPDATE CONNECT EXECUTE CREATE ALTER DROP 
	) },
	'simple_data_type' => { map { ($_ => 1) } qw(
		NUM_INT NUM_EXA NUM_APR STR_BIT STR_CHAR BOOLEAN 
		DATM_FULL DATM_DATE DATM_TIME INTRVL_YM INTRVL_DT 
	) },
	'char_enc_type' => { map { ($_ => 1) } qw(
		UTF8 UTF16 UTF32 ASCII EBCDIC
	) },
	'calendar' => { map { ($_ => 1) } qw(
		ABS GRE JUL CHI HEB ISL JPN
	) },
	'table_index_type' => { map { ($_ => 1) } qw(
		ATOMIC FULLTEXT UNIQUE FOREIGN UFOREIGN
	) },
	'view_type' => { map { ($_ => 1) } qw(
		MATCH SINGLE MULTIPLE COMPOUND SUBQUERY RECURSIVE
	) },
	'compound_operator' => { map { ($_ => 1) } qw(
		UNION DIFFERENCE INTERSECTION EXCLUSION
	) },
	'join_operator' => { map { ($_ => 1) } qw(
		CROSS INNER LEFT RIGHT FULL
	) },
	'view_part' => { map { ($_ => 1) } qw(
		RESULT INTO SET FROM WHERE GROUP HAVING WINDOW ORDER MAXR SKIPR
	) },
	'basic_expr_type' => { map { ($_ => 1) } qw(
		LIT CAST COL MCOL VARG ARG VAR SEQN CVIEW SFUNC UFUNC LIST
	) },
	'standard_func' => { map { ($_ => 1) } qw(
		NOT AND OR XOR
		EQ NE LT GT LE GE IS_NULL NOT_NULL COALESCE SWITCH LIKE
		ADD SUB MUL DIV DIVI MOD ROUND ABS POWER LOG
		SCONCAT SLENGTH SINDEX SUBSTR SREPEAT STRIM SPAD SPADL LC UC
		COUNT MIN MAX SUM AVG CONCAT EVERY ANY SOME EXISTS
		GB_SETS GB_RLUP GB_CUBE
	) },
	'basic_var_type' => { map { ($_ => 1) } qw(
		SCALAR RECORD ARRAY CURSOR
	) },
	'routine_type' => { map { ($_ => 1) } qw(
		ANONYMOUS PACKAGE TRIGGER PROCEDURE FUNCTION BLOCK
	) },
	'basic_trigger_event' => { map { ($_ => 1) } qw(
		BEFR_INS AFTR_INS INST_INS 
		BEFR_UPD AFTR_UPD INST_UPD 
		BEFR_DEL AFTR_DEL INST_DEL
	) },
	'basic_stmt_type' => { map { ($_ => 1) } qw(
		BLOCK ASSIGN RETURN SPROC UPROC
	) },
	'standard_proc' => { map { ($_ => 1) } qw(
		CURSOR_OPEN CURSOR_CLOSE CURSOR_FETCH SELECT_INTO
		INSERT UPDATE DELETE 
		COMMIT ROLLBACK
		LOCK UNLOCK 
		PLAIN THROW TRY CATCH IF ELSEIF ELSE SWITCH CASE OTHERWISE FOREACH 
		FOR WHILE UNTIL MAP GREP REGEXP 
		LOOP CONDITION LOGIC 
	) },
	'user_type' => { map { ($_ => 1) } qw(
		ROOT SCHEMA_OWNER DATA_EDITOR ANONYMOUS
	) },
	'command_type' => { map { ($_ => 1) } qw(
		DB_LIST DB_INFO DB_VERIFY DB_CREATE DB_DELETE DB_CLONE DB_MOVE
		DB_OPEN 
		DB_CLOSE 
		DB_PING DB_ATTACH DB_DETACH 
		TRA_OPEN 
		TRA_CLOSE
		SCHEMA_LIST SCHEMA_INFO SCHEMA_VERIFY
		SCHEMA_CREATE SCHEMA_DELETE SCHEMA_CLONE SCHEMA_UPDATE 
		DOMAIN_LIST DOMAIN_INFO DOMAIN_VERIFY
		DOMAIN_CREATE DOMAIN_DELETE DOMAIN_CLONE DOMAIN_UPDATE
		SEQU_LIST SEQU_INFO SEQU_VERIFY
		SEQU_CREATE SEQU_DELETE SEQU_CLONE SEQU_UPDATE
		TABLE_LIST TABLE_INFO TABLE_VERIFY
		TABLE_CREATE TABLE_DELETE TABLE_CLONE TABLE_UPDATE
		VIEW_LIST VIEW_INFO VIEW_VERIFY
		VIEW_CREATE VIEW_DELETE VIEW_CLONE VIEW_UPDATE
		ROUTINE_LIST ROUTINE_INFO ROUTINE_VERIFY 
		ROUTINE_CREATE ROUTINE_DELETE ROUTINE_CLONE ROUTINE_UPDATE
		USER_LIST USER_INFO USER_VERIFY
		USER_CREATE USER_DELETE USER_CLONE USER_UPDATE USER_GRANT USER_REVOKE
		REC_FETCH 
		REC_VERIFY REC_INSERT REC_UPDATE 
		REC_DELETE REC_REPLACE REC_CLONE REC_LOCK REC_UNLOCK
		CALL_PROC CALL_FUNC
	) },
);

# Names of hash keys in %NODE_TYPES elements:
my $TPI_AT_SEQUENCE  = 'at_sequence'; # Array of all 'attribute' names in canon order
my $TPI_AT_LITERALS  = 'at_literals'; # Hash - Keys are attr names a Node can have which have literal values
	# Values are enums and say what literal data type the attribute has, like int or bool or str
my $TPI_AT_ENUMS     = 'at_enums'; # Hash - Keys are attr names a Node can have which are enumerated values
	# Values are enums and match a %ENUMERATED_TYPES key
my $TPI_AT_NREFS     = 'at_nrefs'; # Hash - Keys are attr names a Node can have which are Node Ref/Id values
	# Values are enums and each matches a single %NODE_TYPES key.
my $TPI_P_NODE_ATNMS = 'p_node_atnms'; # Array whose elements match keys of AT_NREFS (P_NODE_ATNMS is a list subset)
my $TPI_P_PSEUDONODE = 'p_pseudonode'; # If set, Nodes of this type have a hard-coded pseudo-parent
my $TPI_MA_ATTRS     = 'ma_attrs'; # Array of always-mandatory ('MA') attributes
	# The array contains 3 elements, one each for lit, enum, nref; each inner elem is a MA boolean
my $TPI_MUTEX_ATGPS  = 'mutex_atgps'; # Array of groups of mutually exclusive attributes
	# Each array element is an array ref with 5 elements: 1. mutex-name (cstr); 2. lit members (ary); 
	# 3. enum members (ary); 4. nref members (ary); 5. mandatory-flag (boolean).
my $TPI_LOCAL_ATDPS  = 'local_atdps'; # Array of attributes depended-on by other attrs in same Nodes
	# Each array element is an array ref with 4 elements: 
	# 1. undef or depended on lit attr name (cstr); 2. undef or depended on enum attr name (cstr); 
	# 3. undef or depended on nref attr name (cstr); 4. an array ref of N elements where 
	# each element is an array ref with 5 elements: 
		# 1. an array ref with 0..N elements that are names of dependent lit attrs; 
		# 2. an array ref with 0..N elements that are names of dependent enum attrs; 
		# 3. an array ref with 0..N elements that are names of dependent nref attrs; 
		# 4. an array ref with 0..N elements that are depended-on values, one of which must 
		# be matched, if depended-on attr is an enum, or which is empty otherwise;
		# 5. mandatory-flag (boolean).

# Names of special "pseudo-nodes" that are used in an XML version of this structure.
my $SQLSM_L1_ROOT_PSND = 'root';
my $SQLSM_L2_ELEM_PSND = 'elements';
my $SQLSM_L2_BLPR_PSND = 'blueprints';
my $SQLSM_L2_TOOL_PSND = 'tools';
my $SQLSM_L2_SITE_PSND = 'sites';
my $SQLSM_L2_CIRC_PSND = 'circumventions';
my @L2_PSEUDONODE_LIST = ($SQLSM_L2_ELEM_PSND, $SQLSM_L2_BLPR_PSND, 
	$SQLSM_L2_TOOL_PSND, $SQLSM_L2_SITE_PSND, $SQLSM_L2_CIRC_PSND);

# These are the allowed Node types, with their allowed attributes and their 
# allowed child Node types.  They are used for method input checking and 
# other related tasks.
my %NODE_TYPES = (
	'catalog' => {
		$TPI_AT_SEQUENCE => [qw( 
			id name 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_BLPR_PSND,
	},
	'application' => {
		$TPI_AT_SEQUENCE => [qw( 
			id name 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_BLPR_PSND,
	},
	'owner' => {
		$TPI_AT_SEQUENCE => [qw( 
			id catalog 
		)],
		$TPI_AT_NREFS => {
			'catalog' => 'catalog',
		},
		$TPI_P_NODE_ATNMS => [qw( catalog )],
	},
	'catalog_link' => {
		$TPI_AT_SEQUENCE => [qw( 
			id catalog application name target
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'catalog' => 'catalog',
			'application' => 'application',
			'target' => 'catalog',
		},
		$TPI_P_NODE_ATNMS => [qw( catalog application )],
		$TPI_MA_ATTRS => [[qw( name )],[],[qw( target )]],
	},
	'schema' => {
		$TPI_AT_SEQUENCE => [qw( 
			id catalog name owner 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'catalog' => 'catalog',
			'owner' => 'owner',
		},
		$TPI_P_NODE_ATNMS => [qw( catalog )],
		$TPI_MA_ATTRS => [[qw( name )],[],[qw( owner )]],
	},
	'role' => {
		$TPI_AT_SEQUENCE => [qw( 
			id catalog name
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'catalog' => 'catalog',
		},
		$TPI_P_NODE_ATNMS => [qw( catalog )],
		$TPI_MA_ATTRS => [[qw( name )],[],[]],
	},
	'privilege_on' => {
		$TPI_AT_SEQUENCE => [qw( 
			id role schema domain sequence table routine
		)],
		$TPI_AT_NREFS => {
			'role' => 'role',
			'schema' => 'schema',
			'domain' => 'domain',
			'sequence' => 'sequence',
			'table' => 'table',
			'routine' => 'routine',
		},
		$TPI_P_NODE_ATNMS => [qw( role )],
		$TPI_MUTEX_ATGPS => [
			['privilege_on',[],[],[qw( schema domain sequence table routine )],1],
		],
	},
	'privilege_for' => {
		$TPI_AT_SEQUENCE => [qw( 
			id priv_on priv_type
		)],
		$TPI_AT_ENUMS => {
			'priv_type' => 'privilege_type',
		},
		$TPI_AT_NREFS => {
			'priv_on' => 'privilege_on',
		},
		$TPI_P_NODE_ATNMS => [qw( priv_on )],
		$TPI_MA_ATTRS => [[],[qw( priv_type )],[]],
	},
	'domain' => {
		$TPI_AT_SEQUENCE => [qw( 
			id schema name base_type num_precision num_scale num_octets num_unsigned 
			max_octets max_chars store_fixed char_enc trim_white uc_latin lc_latin 
			pad_char trim_pad calendar with_zone range_min range_max 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'num_precision' => 'uint',
			'num_scale' => 'uint',
			'num_octets' => 'uint',
			'num_unsigned' => 'bool',
			'max_octets' => 'uint',
			'max_chars' => 'uint',
			'store_fixed' => 'bool',
			'trim_white' => 'bool',
			'uc_latin' => 'bool',
			'lc_latin' => 'bool',
			'pad_char' => 'cstr',
			'trim_pad' => 'bool',
			'with_zone' => 'sint',
			'range_min' => 'misc',
			'range_max' => 'misc',
		},
		$TPI_AT_ENUMS => {
			'base_type' => 'simple_data_type',
			'char_enc' => 'char_enc_type',
			'calendar' => 'calendar',
		},
		$TPI_AT_NREFS => {
			'schema' => 'schema',
		},
		$TPI_P_NODE_ATNMS => [qw( schema )],
		$TPI_MA_ATTRS => [[qw( name )],[qw( base_type )],[]],
		$TPI_MUTEX_ATGPS => [
			['num_size',[qw( num_precision num_octets )],[],[],0],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'base_type',undef,[
				[['num_precision'],[],[],['NUM_INT','NUM_EXA','NUM_APR'],0],
				[['num_scale'],[],[],['NUM_EXA','NUM_APR'],0],
				[['num_octets'],[],[],['NUM_INT','NUM_APR'],0],
				[['num_unsigned'],[],[],['NUM_INT','NUM_EXA','NUM_APR'],0],
				[['max_octets'],[],[],['STR_BIT'],1],
				[['max_chars'],[],[],['STR_CHAR'],1],
				[[],['char_enc'],[],['STR_CHAR'],1],
				[['trim_white'],[],[],['STR_CHAR'],0],
				[['uc_latin','lc_latin'],[],[],['STR_CHAR'],0],
				[['pad_char'],[],[],['STR_CHAR'],0],
				[['trim_pad'],[],[],['STR_CHAR'],0],
				[[],['calendar'],[],['DATM_FULL','DATM_DATE'],1],
				[['with_zone'],[],[],['DATM_FULL','DATM_DATE','DATM_TIME'],0],
			]],
			['num_precision',undef,undef,[
				[['num_scale'],[],[],[],0],
			]],
		],
	},
	'domain_opt' => {
		$TPI_AT_SEQUENCE => [qw( 
			id domain value 
		)],
		$TPI_AT_LITERALS => {
			'value' => 'misc',
		},
		$TPI_AT_NREFS => {
			'domain' => 'domain',
		},
		$TPI_P_NODE_ATNMS => [qw( domain )],
		$TPI_MA_ATTRS => [[qw( value )],[],[]],
	},
	'sequence' => {
		$TPI_AT_SEQUENCE => [qw( 
			id schema name increment min_val max_val start_val cycle order 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'increment' => 'sint',
			'min_val' => 'sint',
			'max_val' => 'sint',
			'start_val' => 'sint',
			'cycle' => 'bool',
			'order' => 'bool',
		},
		$TPI_AT_NREFS => {
			'schema' => 'schema',
		},
		$TPI_P_NODE_ATNMS => [qw( schema )],
		$TPI_MA_ATTRS => [[qw( name )],[],[]],
	},
	'table' => {
		$TPI_AT_SEQUENCE => [qw( 
			id schema name 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'schema' => 'schema',
		},
		$TPI_P_NODE_ATNMS => [qw( schema )],
		$TPI_MA_ATTRS => [[qw( name )],[],[]],
	},
	'table_col' => {
		$TPI_AT_SEQUENCE => [qw( 
			id table name domain mandatory default_val auto_inc default_seq 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'mandatory' => 'bool',
			'default_val' => 'cstr',
			'auto_inc' => 'bool',
		},
		$TPI_AT_NREFS => {
			'table' => 'table',
			'domain' => 'domain',
			'default_seq' => 'sequence',
		},
		$TPI_P_NODE_ATNMS => [qw( table )],
		$TPI_MA_ATTRS => [[qw( name mandatory )],[],[qw( domain )]],
		$TPI_MUTEX_ATGPS => [
			['default',[qw( default_val )],[],[qw( default_seq )],0],
		],
	},
	'table_ind' => {
		$TPI_AT_SEQUENCE => [qw( 
			id table name ind_type f_table 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'ind_type' => 'table_index_type',
		},
		$TPI_AT_NREFS => {
			'table' => 'table',
			'f_table' => 'table',
		},
		$TPI_P_NODE_ATNMS => [qw( table )],
		$TPI_MA_ATTRS => [[qw( name )],[qw( ind_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'ind_type',undef,[
				[[],[],['f_table'],['FOREIGN','UFOREIGN'],1],
			]],
		],
	},
	'table_ind_col' => {
		$TPI_AT_SEQUENCE => [qw( 
			id table_ind table_col f_table_col 
		)],
		$TPI_AT_NREFS => {
			'table_ind' => 'table_ind',
			'table_col' => 'table_col',
			'f_table_col' => 'table_col',
		},
		$TPI_P_NODE_ATNMS => [qw( table_ind )],
		$TPI_MA_ATTRS => [[],[],[qw( table_col )]],
	},
	'view' => {
		$TPI_AT_SEQUENCE => [qw( 
			id view_type schema name application routine p_view 
			match_all_cols compound_op distinct_rows may_write 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'match_all_cols' => 'bool',
			'distinct_rows' => 'bool',
			'may_write' => 'bool',
		},
		$TPI_AT_ENUMS => {
			'view_type' => 'view_type',
			'compound_op' => 'compound_operator',
		},
		$TPI_AT_NREFS => {
			'schema' => 'schema',
			'application' => 'application',
			'routine' => 'routine',
			'p_view' => 'view',
		},
		$TPI_P_NODE_ATNMS => [qw( schema application routine p_view )],
		$TPI_MA_ATTRS => [[qw( name )],[qw( view_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'view_type',undef,[
				[['match_all_cols'],[],[],['MATCH'],0],
				[[],['compound_op'],[],['COMPOUND'],1],
			]],
		],
	},
	'view_arg' => {
		$TPI_AT_SEQUENCE => [qw( 
			id view name domain 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'view' => 'view',
			'domain' => 'domain',
		},
		$TPI_P_NODE_ATNMS => [qw( view )],
		$TPI_MA_ATTRS => [[qw( name )],[],[qw( domain )]],
	},
	'view_src' => {
		$TPI_AT_SEQUENCE => [qw( 
			id view name match_table match_view catalog_link may_write
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'may_write' => 'bool',
		},
		$TPI_AT_NREFS => {
			'view' => 'view',
			'match_table' => 'table',
			'match_view' => 'view',
			'catalog_link' => 'catalog_link',
		},
		$TPI_P_NODE_ATNMS => [qw( view )],
		$TPI_MA_ATTRS => [[qw( name )],[],[]],
		$TPI_MUTEX_ATGPS => [
			['match',[],[],[qw( match_table match_view )],1],
		],
	},
	'view_src_arg' => {
		$TPI_AT_SEQUENCE => [qw( 
			id src match_view_arg
		)],
		$TPI_AT_NREFS => {
			'src' => 'view_src',
			'match_view_arg' => 'view_arg',
		},
		$TPI_P_NODE_ATNMS => [qw( src )],
		$TPI_MA_ATTRS => [[],[],[qw( match_view_arg )]],
	},
	'view_src_col' => {
		$TPI_AT_SEQUENCE => [qw( 
			id src match_table_col match_view_col
		)],
		$TPI_AT_NREFS => {
			'src' => 'view_src',
			'match_table_col' => 'table_col',
			'match_view_col' => 'view_col',
		},
		$TPI_P_NODE_ATNMS => [qw( src )],
		$TPI_MUTEX_ATGPS => [
			['match_col',[],[],[qw( match_table_col match_view_col )],1],
		],
	},
	'view_col' => {
		$TPI_AT_SEQUENCE => [qw( 
			id view name domain src_col 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'view' => 'view',
			'domain' => 'domain',
			'src_col' => 'view_src_col',
		},
		$TPI_P_NODE_ATNMS => [qw( view )],
		$TPI_MA_ATTRS => [[qw( name )],[],[qw( domain )]],
	},
	'view_join' => {
		$TPI_AT_SEQUENCE => [qw( 
			id view lhs_src rhs_src join_type 
		)],
		$TPI_AT_ENUMS => {
			'join_type' => 'join_operator',
		},
		$TPI_AT_NREFS => {
			'view' => 'view',
			'lhs_src' => 'view_src',
			'rhs_src' => 'view_src',
		},
		$TPI_P_NODE_ATNMS => [qw( view )],
		$TPI_MA_ATTRS => [[],[qw( join_type )],[qw( lhs_src rhs_src )]],
	},
	'view_join_col' => {
		$TPI_AT_SEQUENCE => [qw( 
			id join lhs_src_col rhs_src_col 
		)],
		$TPI_AT_NREFS => {
			'join' => 'view_join',
			'lhs_src_col' => 'view_src_col',
			'rhs_src_col' => 'view_src_col',
		},
		$TPI_P_NODE_ATNMS => [qw( join )],
		$TPI_MA_ATTRS => [[],[],[qw( lhs_src_col rhs_src_col )]],
	},
	'view_hierarchy' => {
		$TPI_AT_SEQUENCE => [qw( 
			id view start_src_col start_expr_type 
			start_lit_val start_view_arg start_routine_arg start_routine_var
			conn_src_col p_conn_src_col 
		)],
		$TPI_AT_LITERALS => {
			'start_lit_val' => 'misc',
		},
		$TPI_AT_ENUMS => {
			'start_expr_type' => 'basic_expr_type',
		},
		$TPI_AT_NREFS => {
			'view' => 'view',
			'start_src_col' => 'view_src_col',
			'start_view_arg' => 'view_arg',
			'start_routine_arg' => 'routine_arg',
			'start_routine_var' => 'routine_var',
			'conn_src_col' => 'view_src_col',
			'p_conn_src_col' => 'view_src_col',
		},
		$TPI_P_NODE_ATNMS => [qw( view )],
		$TPI_MA_ATTRS => [[],[],[qw( start_src_col conn_src_col p_conn_src_col )]],
		$TPI_MUTEX_ATGPS => [
			['start_val',[qw( start_lit_val )],[],
				[qw( start_view_arg start_routine_arg start_routine_var )],1],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'start_expr_type',undef,[
				[[],[],['start_lit_val'],['LIT'],1],
				[[],[],['start_view_arg'],['VARG'],1],
				[[],[],['start_routine_arg'],['ARG'],1],
				[[],[],['start_routine_var'],['VAR'],1],
			]],
		],
	},
	'view_expr' => {
		$TPI_AT_SEQUENCE => [qw( 
			id expr_type p_expr view view_part 
			view_col set_routine_arg set_routine_var set_view_col view_src_arg 
			domain lit_val src_col match_col view_arg routine_arg routine_var sequence 
			call_view call_view_arg call_sfunc call_ufunc call_ufunc_arg catalog_link
		)],
		$TPI_AT_LITERALS => {
			'lit_val' => 'misc',
		},
		$TPI_AT_ENUMS => {
			'expr_type' => 'basic_expr_type',
			'view_part' => 'view_part',
			'call_sfunc' => 'standard_func',
		},
		$TPI_AT_NREFS => {
			'p_expr' => 'view_expr',
			'view' => 'view',
			'view_col' => 'view_col',
			'set_routine_arg' => 'routine_arg',
			'set_routine_var' => 'routine_var',
			'set_view_col' => 'view_src_col',
			'view_src_arg' => 'view_src_arg',
			'domain' => 'domain',
			'src_col' => 'view_src_col',
			'match_col' => 'view_col',
			'view_arg' => 'view_arg',
			'routine_arg' => 'routine_arg',
			'routine_var' => 'routine_var',
			'sequence' => 'sequence',
			'call_view' => 'view',
			'call_view_arg' => 'view_arg',
			'call_ufunc' => 'routine',
			'call_ufunc_arg' => 'routine_arg',
			'catalog_link' => 'catalog_link',
		},
		$TPI_P_NODE_ATNMS => [qw( p_expr view )],
		$TPI_MA_ATTRS => [[],[qw( expr_type )],[]],
		$TPI_MUTEX_ATGPS => [
			['expr_root_view_part',[],[qw( view_part )],[qw( p_expr )],1],
			['value',[qw( lit_val )],[qw( call_sfunc )],
				[qw( src_col match_col view_arg routine_arg routine_var sequence
				call_view call_view_arg call_ufunc call_ufunc_arg )],1],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'view_part',undef,[
				[[],[],['view_col'],['RESULT','INTO'],1],
				[[],[],['set_routine_arg','set_routine_var'],['INTO'],1],
				[[],[],['set_view_col'],['SET'],1],
				[[],[],['view_src_arg'],['FROM'],1],
			]],
			[undef,'expr_type',undef,[
				[[],[],['domain'],['LIT','CAST'],1],
				[['lit_val'],[],[],['LIT'],1],
				[[],[],['src_col'],['COL'],1],
				[[],[],['match_col'],['MCOL'],1],
				[[],[],['view_arg'],['VARG'],1],
				[[],[],['routine_arg'],['ARG'],1],
				[[],[],['routine_var'],['VAR'],1],
				[[],[],['sequence'],['SEQN'],1],
				[[],[],['call_view'],['CVIEW'],1],
				[[],['call_sfunc'],[],['SFUNC'],1],
				[[],[],['call_ufunc'],['UFUNC'],1],
			]],
			[undef,undef,'call_ufunc',[
				[[],[],['catalog_link'],[],0],
			]],
		],
	},
	'routine' => {
		$TPI_AT_SEQUENCE => [qw( 
			id routine_type schema application p_routine table view 
			name return_var_type return_domain trigger_event trigger_per_stmt
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'trigger_per_stmt' => 'bool',
		},
		$TPI_AT_ENUMS => {
			'routine_type' => 'routine_type',
			'return_var_type' => 'basic_var_type',
			'trigger_event' => 'basic_trigger_event',
		},
		$TPI_AT_NREFS => {
			'schema' => 'schema',
			'application' => 'application',
			'p_routine' => 'routine',
			'table' => 'table',
			'view' => 'view',
			'return_domain' => 'domain',
		},
		$TPI_P_NODE_ATNMS => [qw( schema application p_routine table view )],
		$TPI_MA_ATTRS => [[qw( name )],[qw( routine_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'routine_type',undef,[
				[[],['return_var_type'],[],['ANONYMOUS','FUNCTION'],0], # TODO: make mandatory for FUNCTION only
				[[],['trigger_event'],[],['TRIGGER'],1],
				[['trigger_per_stmt'],[],[],['TRIGGER'],1],
			]],
			[undef,'return_var_type',undef,[
				[[],[],['return_domain'],['SCALAR'],1],
			]],
		],
	},
	'routine_arg' => {
		$TPI_AT_SEQUENCE => [qw( 
			id routine name var_type domain curs_view 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'var_type' => 'basic_var_type',
		},
		$TPI_AT_NREFS => {
			'routine' => 'routine',
			'domain' => 'domain',
			'curs_view' => 'view',
		},
		$TPI_P_NODE_ATNMS => [qw( routine )],
		$TPI_MA_ATTRS => [[qw( name )],[qw( var_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'var_type',undef,[
				[[],[],['domain'],['SCALAR'],1],
				[[],[],['curs_view'],['CURSOR'],1],
			]],
		],
	},
	'routine_var' => {
		$TPI_AT_SEQUENCE => [qw( 
			id routine name var_type domain init_lit_val is_constant curs_view curs_for_update 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'init_lit_val' => 'misc',
			'is_constant' => 'bool',
			'curs_for_update' => 'bool',
		},
		$TPI_AT_ENUMS => {
			'var_type' => 'basic_var_type',
		},
		$TPI_AT_NREFS => {
			'routine' => 'routine',
			'domain' => 'domain',
			'curs_view' => 'view',
		},
		$TPI_P_NODE_ATNMS => [qw( routine )],
		$TPI_MA_ATTRS => [[qw( name )],[qw( var_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'var_type',undef,[
				[[],[],['domain'],['SCALAR'],1],
				[['init_lit_val'],[],[],['SCALAR'],0],
				[['is_constant'],[],[],['SCALAR'],0],
				[[],[],['curs_view'],['CURSOR'],1],
				[['curs_for_update'],[],[],['CURSOR'],0],
			]],
		],
	},
	'routine_stmt' => {
		$TPI_AT_SEQUENCE => [qw( 
			id routine stmt_type block_routine dest_arg dest_var 
			call_sproc curs_arg curs_var view_for_dml 
			call_uproc catalog_link 
		)],
		$TPI_AT_ENUMS => {
			'stmt_type' => 'basic_stmt_type',
			'call_sproc' => 'standard_proc',
		},
		$TPI_AT_NREFS => {
			'routine' => 'routine',
			'block_routine' => 'routine',
			'dest_arg' => 'routine_arg',
			'dest_var' => 'routine_var',
			'curs_arg' => 'routine_arg',
			'curs_var' => 'routine_var',
			'view_for_dml' => 'view',
			'call_uproc' => 'routine',
			'catalog_link' => 'catalog_link',
		},
		$TPI_P_NODE_ATNMS => [qw( routine )],
		$TPI_MA_ATTRS => [[],[qw( stmt_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'stmt_type',undef,[
				[[],[],['block_routine'],['BLOCK'],1],
				[[],[],['dest_arg','dest_var'],['ASSIGN'],1],
				[[],['call_sproc'],[],['SPROC'],1],
				[[],[],['curs_arg','curs_var'],['SPROC'],0],
				[[],[],['view_for_dml'],['SPROC'],0],
				[[],[],['call_uproc'],['UPROC'],1],
			]],
			[undef,undef,'call_uproc',[
				[[],[],['catalog_link'],[],0],
			]],
		],
	},
	'routine_expr' => {
		$TPI_AT_SEQUENCE => [qw( 
			id expr_type p_expr p_stmt domain lit_val routine_arg routine_var sequence 
			call_sfunc call_ufunc call_ufunc_arg catalog_link
		)],
		$TPI_AT_LITERALS => {
			'lit_val' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'expr_type' => 'basic_expr_type',
			'call_sfunc' => 'standard_func',
		},
		$TPI_AT_NREFS => {
			'p_expr' => 'routine_expr',
			'p_stmt' => 'routine_stmt',
			'domain' => 'domain',
			'routine_arg' => 'routine_arg',
			'routine_var' => 'routine_var',
			'sequence' => 'sequence',
			'call_ufunc' => 'routine',
			'call_ufunc_arg' => 'routine_arg',
			'catalog_link' => 'catalog_link',
		},
		$TPI_P_NODE_ATNMS => [qw( p_expr p_stmt )],
		$TPI_MA_ATTRS => [[],[qw( expr_type )],[]],
		$TPI_MUTEX_ATGPS => [
			['value',[qw( lit_val )],[qw( call_sfunc )],
				[qw( routine_arg routine_var sequence call_ufunc call_ufunc_arg )],1],
		],
		$TPI_LOCAL_ATDPS => [
			[undef,'expr_type',undef,[
				[[],[],['domain'],['LIT','CAST'],1],
				[['lit_val'],[],[],['LIT'],1],
				[[],[],['routine_arg'],['ARG'],1],
				[[],[],['routine_var'],['VAR'],1],
				[[],[],['sequence'],['SEQN'],1],
				[[],['call_sfunc'],[],['SFUNC'],1],
				[[],[],['call_ufunc'],['UFUNC'],1],
			]],
			[undef,undef,'call_ufunc',[
				[[],[],['catalog_link'],[],0],
			]],
		],
	},
	'command' => {
		$TPI_AT_SEQUENCE => [qw( 
			id application name command_type
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'command_type' => 'command_type',
		},
		$TPI_AT_NREFS => {
			'application' => 'application',
		},
		$TPI_P_NODE_ATNMS => [qw( application )],
		$TPI_MA_ATTRS => [[],[qw( command_type )],[]],
	},
	'command_arg' => {
		$TPI_AT_SEQUENCE => [qw( 
			id command catalog_link schema domain sequence table view routine user
		)],
		$TPI_AT_NREFS => {
			'command' => 'command',
			'catalog_link' => 'catalog_link',
			'schema' => 'schema',
			'domain' => 'domain',
			'sequence' => 'sequence',
			'table' => 'table',
			'view' => 'view',
			'routine' => 'routine',
			'user' => 'user',
		},
		$TPI_P_NODE_ATNMS => [qw( command )],
		$TPI_MUTEX_ATGPS => [
			['command_arg',[],[],
				[qw( catalog_link schema domain sequence table view routine user )],1],
		],
	},
	'data_storage_product' => {
		$TPI_AT_SEQUENCE => [qw( 
			id name product_code is_file_based is_local_proc is_network_svc
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'product_code' => 'cstr',
			'is_file_based' => 'bool',
			'is_local_proc' => 'bool',
			'is_network_svc' => 'bool',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_TOOL_PSND,
		$TPI_MA_ATTRS => [[qw( product_code )],[],[]],
		$TPI_MUTEX_ATGPS => [
			['type',[qw( is_file_based is_local_proc is_network_svc )],[],[],1],
		],
	},
	'data_link_product' => {
		$TPI_AT_SEQUENCE => [qw( 
			id name product_code is_proxy
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'product_code' => 'cstr',
			'is_proxy' => 'bool',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_TOOL_PSND,
		$TPI_MA_ATTRS => [[qw( product_code )],[],[]],
	},
	'catalog_instance' => {
		$TPI_AT_SEQUENCE => [qw( 
			id product blueprint name server_ip server_domain server_port file_path
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'server_ip' => 'cstr',
			'server_domain' => 'cstr',
			'server_port' => 'uint',
			'file_path' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'product' => 'data_storage_product',
			'blueprint' => 'catalog',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_SITE_PSND,
		$TPI_MA_ATTRS => [[],[],[qw( product blueprint )]],
	},
	'catalog_instance_opt' => {
		$TPI_AT_SEQUENCE => [qw( 
			id catalog key value 
		)],
		$TPI_AT_LITERALS => {
			'key' => 'cstr',
			'value' => 'misc',
		},
		$TPI_AT_NREFS => {
			'catalog' => 'catalog_instance',
		},
		$TPI_P_NODE_ATNMS => [qw( catalog )],
		$TPI_MA_ATTRS => [[qw( key value )],[],[]],
	},
	'application_instance' => {
		$TPI_AT_SEQUENCE => [qw( 
			id blueprint name 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'blueprint' => 'application',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_SITE_PSND,
		$TPI_MA_ATTRS => [[],[],[qw( blueprint )]],
	},
	'catalog_link_instance' => {
		$TPI_AT_SEQUENCE => [qw( 
			id product p_link catalog application unrealized target local_dsn login_user login_pass
		)],
		$TPI_AT_LITERALS => {
			'local_dsn' => 'cstr',
			'login_user' => 'cstr',
			'login_pass' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'product' => 'data_link_product',
			'p_link' => 'catalog_link_instance',
			'catalog' => 'catalog_instance',
			'application' => 'application_instance',
			'unrealized' => 'catalog_link',
			'target' => 'catalog_instance',
		},
		$TPI_P_NODE_ATNMS => [qw( p_link catalog application )],
		$TPI_MA_ATTRS => [[],[],[qw( product )]],
		$TPI_MUTEX_ATGPS => [
			['link_root_unrealized',[],[],[qw( p_link unrealized )],1],
			['link_root_target',[],[],[qw( p_link target )],1],
		],
	},
	'catalog_link_instance_opt' => {
		$TPI_AT_SEQUENCE => [qw( 
			id link key value 
		)],
		$TPI_AT_LITERALS => {
			'key' => 'cstr',
			'value' => 'misc',
		},
		$TPI_AT_NREFS => {
			'link' => 'catalog_link_instance',
		},
		$TPI_P_NODE_ATNMS => [qw( link )],
		$TPI_MA_ATTRS => [[qw( key value )],[],[]],
	},
	'user' => {
		$TPI_AT_SEQUENCE => [qw( 
			id catalog user_type match_owner name password default_schema 
		)],
		$TPI_AT_LITERALS => {
			'name' => 'cstr',
			'password' => 'cstr',
		},
		$TPI_AT_ENUMS => {
			'user_type' => 'user_type',
		},
		$TPI_AT_NREFS => {
			'catalog' => 'catalog_instance',
			'match_owner' => 'owner',
			'default_schema' => 'schema',
		},
		$TPI_P_NODE_ATNMS => [qw( catalog )],
		$TPI_MA_ATTRS => [[],[qw( user_type )],[]],
		$TPI_LOCAL_ATDPS => [
			[undef,'user_type',undef,[
				[[],[],['match_owner'],['SCHEMA_OWNER'],1],
				[['name'],[],[],['ROOT','SCHEMA_OWNER','DATA_EDITOR'],1],
				[['password'],[],[],['ROOT','SCHEMA_OWNER','DATA_EDITOR'],1],
			]],
		],
	},
	'user_role' => {
		$TPI_AT_SEQUENCE => [qw( 
			id user role 
		)],
		$TPI_AT_NREFS => {
			'user' => 'user',
			'role' => 'role',
		},
		$TPI_P_NODE_ATNMS => [qw( user )],
		$TPI_MA_ATTRS => [[],[],[qw( role )]],
	},
	'sql_fragment' => {
		$TPI_AT_SEQUENCE => [qw( 
			id product att_node_type att_node_id is_inside is_before is_after fragment
		)],
		$TPI_AT_LITERALS => {
			'att_node_type' => 'cstr',
			'att_node_id' => 'uint',
			'is_inside' => 'bool',
			'is_before' => 'bool',
			'is_after' => 'bool',
			'fragment' => 'cstr',
		},
		$TPI_AT_NREFS => {
			'product' => 'data_storage_product',
		},
		$TPI_P_PSEUDONODE => $SQLSM_L2_CIRC_PSND,
		$TPI_MUTEX_ATGPS => [
			['is_where',[qw( is_inside is_before is_after )],[],[],0],
		],
	},
);

# This is an extension to let you use one set of functions for all Node 
# attribute major types, rather than separate literal/enumerated/node.
my $NAMT_ID      = 'ID'; # node id attribute
my $NAMT_LITERAL = 'LITERAL'; # literal attribute
my $NAMT_ENUM    = 'ENUM'; # enumerated attribute
my $NAMT_NODE    = 'NODE'; # node attribute
my $ATTR_ID      = 'id'; # attribute name to use for the node id

# These special hash keys are used by the get_all_properties[/*]() methods:
my $DBG_GAP_NODE_TYPE = 'NODE_TYPE'; # str - what type of Node we are
my $DBG_GAP_ATTRS     = 'ATTRS'; # hash - our attributes, including refs/ids of parents we will have
my $DBG_GAP_CHILDREN  = 'CHILDREN'; # list of refs to new Nodes we will become primary parent of

######################################################################

sub valid_enumerated_types {
	my ($self, $type) = @_;
	$type and return( exists( $ENUMERATED_TYPES{$type} ) );
	return( {map { ($_ => 1) } keys %ENUMERATED_TYPES} );
}

sub valid_enumerated_type_values {
	my ($self, $type, $value) = @_;
	($type and exists( $ENUMERATED_TYPES{$type} )) or return( undef );
	$value and return( exists( $ENUMERATED_TYPES{$type}->{$value} ) );
	return( {%{$ENUMERATED_TYPES{$type}}} );
}

sub valid_node_types {
	my ($self, $type) = @_;
	$type and return( exists( $NODE_TYPES{$type} ) );
	return( {map { ($_ => 1) } keys %NODE_TYPES} );
}

sub valid_node_type_literal_attributes {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_AT_LITERALS} ) or return( undef );
	$attr and return( $NODE_TYPES{$type}->{$TPI_AT_LITERALS}->{$attr} );
	return( {%{$NODE_TYPES{$type}->{$TPI_AT_LITERALS}}} );
}

sub valid_node_type_enumerated_attributes {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_AT_ENUMS} ) or return( undef );
	$attr and return( $NODE_TYPES{$type}->{$TPI_AT_ENUMS}->{$attr} );
	return( {%{$NODE_TYPES{$type}->{$TPI_AT_ENUMS}}} );
}

sub valid_node_type_node_ref_attributes {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_AT_NREFS} ) or return( undef );
	$attr and return( $NODE_TYPES{$type}->{$TPI_AT_NREFS}->{$attr} );
	return( {%{$NODE_TYPES{$type}->{$TPI_AT_NREFS}}} );
}

sub major_type_of_node_type_attribute {
	my ($self, $type, $attr) = @_;
	($type and $self->valid_node_types( $type )) or return( undef );
	defined( $attr ) or return( undef );
	$attr eq $ATTR_ID and return( $NAMT_ID );
	if( $self->valid_node_type_literal_attributes( $type, $attr ) ) {
		return( $NAMT_LITERAL );
	}
	if( $self->valid_node_type_enumerated_attributes( $type, $attr ) ) {
		return( $NAMT_ENUM );
	}
	if( $self->valid_node_type_node_ref_attributes( $type, $attr ) ) {
		return( $NAMT_NODE );
	}
	return( undef );
}

sub valid_node_type_parent_attribute_names {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_P_NODE_ATNMS} ) or return( undef );
	$attr and return( grep { $_ eq $attr } @{$NODE_TYPES{$type}->{$TPI_P_NODE_ATNMS}} );
	return( [@{$NODE_TYPES{$type}->{$TPI_P_NODE_ATNMS}}] );
}

sub node_types_with_pseudonode_parents {
	my ($self, $type) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	$type and return( $NODE_TYPES{$type}->{$TPI_P_PSEUDONODE} );
	return( {map { ($_ => $NODE_TYPES{$type}->{$TPI_P_PSEUDONODE}) } 
		grep { $NODE_TYPES{$type}->{$TPI_P_PSEUDONODE} } keys %NODE_TYPES} );
}

sub mandatory_node_type_literal_attribute_names {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_MA_ATTRS} ) or return( undef );
	$attr and return( grep { $_ eq $attr } @{$NODE_TYPES{$type}->{$TPI_MA_ATTRS}->[0]} );
	return( [@{$NODE_TYPES{$type}->{$TPI_MA_ATTRS}->[0]}] );
}

sub mandatory_node_type_enumerated_attribute_names {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_MA_ATTRS} ) or return( undef );
	$attr and return( grep { $_ eq $attr } @{$NODE_TYPES{$type}->{$TPI_MA_ATTRS}->[1]} );
	return( [@{$NODE_TYPES{$type}->{$TPI_MA_ATTRS}->[1]}] );
}

sub mandatory_node_type_node_ref_attribute_names {
	my ($self, $type, $attr) = @_;
	($type and exists( $NODE_TYPES{$type} )) or return( undef );
	exists( $NODE_TYPES{$type}->{$TPI_MA_ATTRS} ) or return( undef );
	$attr and return( grep { $_ eq $attr } @{$NODE_TYPES{$type}->{$TPI_MA_ATTRS}->[2]} );
	return( [@{$NODE_TYPES{$type}->{$TPI_MA_ATTRS}->[2]}] );
}

######################################################################
# This is a 'protected' method; only sub-classes should invoke it.

sub _serialize_as_perl {
	my ($self, $ind, $node, $pad) = @_;
	$pad ||= '';
	my $padc = $ind ? "" : "$pad\t\t";
	my $node_type = $node->{$DBG_GAP_NODE_TYPE};
	my $attr_seq = $NODE_TYPES{$node_type}->{$TPI_AT_SEQUENCE};
	my $attrs = $node->{$DBG_GAP_ATTRS};
	return( join( '', 
		$pad."{\n",
		$pad."\t'".$DBG_GAP_NODE_TYPE."' => '".$node_type."',\n",
		(scalar(keys %{$attrs}) ? (
			$pad."\t'".$DBG_GAP_ATTRS."' => {\n",
			(map { $pad."\t\t'".$_."' => '".$self->_s_a_p_esc($attrs->{$_})."',\n" } 
				grep { defined( $attrs->{$_} ) } @{$attr_seq}),
			$pad."\t},\n",
		) : ''),
		(scalar(@{$node->{$DBG_GAP_CHILDREN}}) ? (
			$pad."\t'".$DBG_GAP_CHILDREN."' => [\n",
			(map { $self->_serialize_as_perl( $ind,$_,$padc ) } @{$node->{$DBG_GAP_CHILDREN}}),
			$pad."\t],\n",
		) : ''),
		$pad."},\n",
	) );
}

sub _s_a_p_esc {
	my ($self, $text) = @_;
	$text =~ s/\\/\\\\/g;
	$text =~ s/'/\\'/g;
	return( $text );
}

######################################################################
# This is a 'protected' method; only sub-classes should invoke it.

sub _serialize_as_xml {
	my ($self, $ind, $node, $pad) = @_;
	$pad ||= '';
	my $padc = $ind ? "" : "$pad\t";
	my $node_type = $node->{$DBG_GAP_NODE_TYPE};
	my $attr_seq = $NODE_TYPES{$node_type}->{$TPI_AT_SEQUENCE};
	my $attrs = $node->{$DBG_GAP_ATTRS};
	return( join( '', 
		$pad.'<'.$node_type,
		(map { ' '.$_.'="'.$self->_s_a_x_esc($attrs->{$_}).'"' } 
			grep { defined( $attrs->{$_} ) } @{$attr_seq}),
		(scalar(@{$node->{$DBG_GAP_CHILDREN}}) ? (
			'>'."\n",
			(map { $self->_serialize_as_xml( $ind,$_,$padc ) } @{$node->{$DBG_GAP_CHILDREN}}),
			$pad.'</'.$node_type.'>'."\n",
		) : ' />'."\n"),
	) );
}

sub _s_a_x_esc {
	my ($self, $text) = @_;
	$text =~ s/&/&amp;/g;
	$text =~ s/\"/&quot;/g;
	$text =~ s/>/&gt;/g;
	$text =~ s/</&lt;/g;
	return( $text );
}

######################################################################
# This is a 'protected' method; only sub-classes should invoke it.

sub _throw_error_message {
	my ($self, $error_code, $args) = @_;
	# Throws an exception consisting of an object.  A Container property is not 
	# used to store object so things work properly in multi-threaded environment; 
	# an exception is only supposed to affect the thread that calls it.
	die Locale::KeyedText->new_message( $error_code, $args );
}

######################################################################
# These are convenience wrapper methods.

sub new_container {
	return( SQL::SyntaxModel::Container->new() );
}

sub new_node {
	return( SQL::SyntaxModel::Node->new( $_[1] ) );
}

######################################################################
######################################################################

package SQL::SyntaxModel::Container;
use base qw( SQL::SyntaxModel );

######################################################################

sub new {
	my ($class) = @_;
	my $container = bless( {}, ref($class) || $class );
	$container->{$CPROP_ALL_NODES} = { map { ($_ => {}) } keys %NODE_TYPES };
	$container->{$CPROP_PSEUDONODES} = { map { ($_ => []) } @L2_PSEUDONODE_LIST };
	$container->{$CPROP_NEXT_FREE_NIDS} = { map { ($_ => 1) } keys %NODE_TYPES };
	$container->{$CPROP_DEF_CON_TESTED} = 1;
	return( $container );
}

######################################################################

sub destroy {
	# Since we probably have circular refs, we must explicitly be destroyed.
	my ($container) = @_;
	foreach my $nodes_by_type (values %{$container->{$CPROP_ALL_NODES}}) {
		foreach my $node (values %{$nodes_by_type}) {
			%{$node} = ();
		}
	}
	%{$container} = ();
}

######################################################################

sub get_node {
	my ($container, $node_type, $node_id) = @_;
	defined( $node_type ) or $container->_throw_error_message( 'SSM_C_GET_NODE_NO_ARG_TYPE' );
	defined( $node_id ) or $container->_throw_error_message( 'SSM_C_GET_NODE_NO_ARG_ID' );
	unless( $NODE_TYPES{$node_type} ) {
		$container->_throw_error_message( 'SSM_C_GET_NODE_BAD_TYPE', { 'TYPE' => $node_type } );
	}
	return( $container->{$CPROP_ALL_NODES}->{$node_type}->{$node_id} );
}

######################################################################

sub get_child_nodes {
	my ($container, $node_type) = @_;
	my $pseudonodes = $container->{$CPROP_PSEUDONODES};
	if( defined( $node_type ) ) {
		unless( $NODE_TYPES{$node_type} ) {
			$container->_throw_error_message( 'SSM_C_GET_CH_NODES_BAD_TYPE', { 'TYPE' => $node_type } );
		}
		my $p_pseudonode = $NODE_TYPES{$node_type}->{$TPI_P_PSEUDONODE} or return( [] );
		return( [grep { $_->{$NPROP_NODE_TYPE} eq $node_type } @{$pseudonodes->{$p_pseudonode}}] );
	} else {
		return( [map { @{$pseudonodes->{$_}} } @L2_PSEUDONODE_LIST] );
	}
}

######################################################################

sub get_next_free_node_id {
	my ($container, $node_type) = @_;
	defined( $node_type ) or $container->_throw_error_message( 'SSM_C_GET_NFNI_NO_ARG_TYPE' );
	unless( $NODE_TYPES{$node_type} ) {
		$container->_throw_error_message( 'SSM_C_GET_NFNI_BAD_TYPE', { 'TYPE' => $node_type } );
	}
	return( $container->{$CPROP_NEXT_FREE_NIDS}->{$node_type} );
}

######################################################################

sub deferrable_constraints_are_tested {
	return( $_[0]->{$CPROP_DEF_CON_TESTED} );
}

sub test_deferrable_constraints {
	my ($container) = @_;
	if( $container->{$CPROP_DEF_CON_TESTED} ) {
		return( 1 );
	}
	# Test nodes in the same order that they appear in the Node tree.
	foreach my $pseudonode (@L2_PSEUDONODE_LIST) {
		foreach my $child_node (@{$container->{$CPROP_PSEUDONODES}->{$pseudonode}}) {
			$container->_test_deferrable_constraints( $child_node );
		}
	}
	$container->{$CPROP_DEF_CON_TESTED} = 1;
}

sub _test_deferrable_constraints {
	my ($container, $node) = @_;
	$node->test_deferrable_constraints();
	my %children_were_output = ();
	foreach my $child_node (@{$node->{$NPROP_CHILD_NODES}}) {
		if( my $child_p_node_atnm = $child_node->{$NPROP_P_NODE_ATNM} ) {
			if( my $child_main_parent = $child_node->{$NPROP_AT_NREFS}->{$child_p_node_atnm} ) {
				if( $child_main_parent eq $node ) {
					# Only nav to child if we are its primary parent, not simply any parent.
					unless( $children_were_output{$child_node} ) {
						# Only nav to child once; a child may link to same parent multiple times.
						$container->_test_deferrable_constraints( $child_node );
						$children_were_output{$child_node} = 1;
					}
				}
			}
		} else { # !$child_node->{$NPROP_P_NODE_ATNM}
			# Make sure to report error condition that primary parent attribute name not set, 
			# assuming this Node can't alternately have a pseudonode primary parent.
			$child_node->test_deferrable_constraints();
		}
	}
}

######################################################################

sub get_all_properties {
	return( $_[0]->_get_all_properties() );
}

sub _get_all_properties {
	my ($container) = @_;
	my $pseudonodes = $container->{$CPROP_PSEUDONODES};
	return( {
		$DBG_GAP_NODE_TYPE => $SQLSM_L1_ROOT_PSND,
		$DBG_GAP_ATTRS => {},
		$DBG_GAP_CHILDREN => [map { {
			$DBG_GAP_NODE_TYPE => $_,
			$DBG_GAP_ATTRS => {},
			$DBG_GAP_CHILDREN => [map { $_->_get_all_properties() } @{$pseudonodes->{$_}}],
		} } @L2_PSEUDONODE_LIST],
	} );
}

sub get_all_properties_as_perl_str {
	return( $_[0]->_serialize_as_perl( $_[1], $_[0]->_get_all_properties() ) );
}

sub get_all_properties_as_xml_str {
	return( $_[0]->_serialize_as_xml( $_[1], $_[0]->_get_all_properties() ) );
}

######################################################################
######################################################################

package SQL::SyntaxModel::Node;
use base qw( SQL::SyntaxModel );

######################################################################

sub new {
	my ($class, $node_type) = @_;
	my $node = bless( {}, ref($class) || $class );

	defined( $node_type ) or $node->_throw_error_message( 'SSM_N_NEW_NODE_NO_ARGS' );
	my $type_info = $NODE_TYPES{$node_type};
	unless( $type_info ) {
		$node->_throw_error_message( 'SSM_N_NEW_NODE_BAD_TYPE', { 'TYPE' => $node_type } );
	}

	$node->{$NPROP_NODE_TYPE} = $node_type;
	$node->{$NPROP_NODE_ID} = undef;
	$node->{$NPROP_AT_LITERALS} = {};
	$node->{$NPROP_AT_ENUMS} = {};
	$node->{$NPROP_AT_NREFS} = {};
	$node->{$NPROP_P_NODE_ATNM} = undef;
	$node->{$NPROP_CONTAINER} = undef;
	$node->{$NPROP_LINKS_RECIP} = 0;
	$node->{$NPROP_CHILD_NODES} = [];

	return( $node );
}

######################################################################

sub delete_node {
	my ($node) = @_;

	if( $node->{$NPROP_CONTAINER} ) {
		$node->_throw_error_message( 'SSM_N_DEL_NODE_IN_CONT' );
	}

	# Ultimately the pure-Perl version of this method is a no-op because once 
	# a Node is not in a Container, there are no references to it by any 
	# SQL::SyntaxModel/::* object; it will vanish when external refs go away.
	# This function is a placeholder for the C version, which will require 
	# explicit memory deallocation.
}

######################################################################

sub get_node_type {
	return( $_[0]->{$NPROP_NODE_TYPE} );
}

######################################################################

sub get_node_id {
	return( $_[0]->{$NPROP_NODE_ID} );
}

sub clear_node_id {
	my ($node) = @_;
	if( $node->{$NPROP_CONTAINER} ) {
		$node->_throw_error_message( 'SSM_N_CLEAR_NODE_ID_IN_CONT', 
			{ 'ID' => $node->{$NPROP_NODE_ID}, 'TYPE' => $node->{$NPROP_NODE_TYPE} } );
	}
	$node->{$NPROP_NODE_ID} = undef;
}

sub set_node_id {
	my ($node, $new_id) = @_;
	defined( $new_id ) or $node->_throw_error_message( 'SSM_N_SET_NODE_ID_NO_ARGS' );

	if( $new_id =~ /\D/ or $new_id < 1 or int($new_id) ne $new_id ) {
		# The regexp above should suppress warnings about non-numerical arguments to '<'
		$node->_throw_error_message( 'SSM_N_SET_NODE_ID_BAD_ARG', { 'ARG' => $new_id } );
	}

	if( !$node->{$NPROP_CONTAINER} ) {
		$node->{$NPROP_NODE_ID} = $new_id;
		return( 1 );
	}

	# We would never get here if $node didn't also have a NODE_ID
	my $old_id = $node->{$NPROP_NODE_ID};

	if( $new_id == $old_id ) {
		return( 1 ); # no-op; new id same as old
	}
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $rh_cnl_ft = $node->{$NPROP_CONTAINER}->{$CPROP_ALL_NODES}->{$node_type};

	if( $rh_cnl_ft->{$new_id} ) {
		$node->_throw_error_message( 'SSM_N_SET_NODE_ID_DUPL_ID', 
			{ 'ID' => $new_id, 'TYPE' => $node_type } );
	}

	# The following seq should leave state consistant or recoverable if the thread dies
	$rh_cnl_ft->{$new_id} = $node; # temp reserve new+old
	$node->{$NPROP_NODE_ID} = $new_id; # change self from old to new
	delete( $rh_cnl_ft->{$old_id} ); # now only new reserved
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}

	# Now adjust our "next free node id" counter if appropriate
	my $rh_cnfni = $node->{$NPROP_CONTAINER}->{$CPROP_NEXT_FREE_NIDS};
	if( $new_id >= $rh_cnfni->{$node_type} ) {
		$rh_cnfni->{$node_type} = 1 + $new_id;
	}
}

######################################################################

sub expected_literal_attribute_type {
	my ($node, $attr_name) = @_;
	defined( $attr_name ) or $node->_throw_error_message( 'SSM_N_EXP_LIT_AT_NO_ARGS' );
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $exp_lit_type = $NODE_TYPES{$node_type}->{$TPI_AT_LITERALS} && 
		$NODE_TYPES{$node_type}->{$TPI_AT_LITERALS}->{$attr_name};
	unless( $exp_lit_type ) {
		$node->_throw_error_message( 'SSM_N_EXP_LIT_AT_INVAL_NM', 
			{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type } );
	}
	return( $exp_lit_type );
}

sub get_literal_attribute {
	my ($node, $attr_name) = @_;
	$node->expected_literal_attribute_type( $attr_name ); # dies if bad arg
	return( $node->{$NPROP_AT_LITERALS}->{$attr_name} );
}

sub get_literal_attributes {
	return( {%{$_[0]->{$NPROP_AT_LITERALS}}} );
}

sub clear_literal_attribute {
	my ($node, $attr_name) = @_;
	$node->expected_literal_attribute_type( $attr_name ); # dies if bad arg
	delete( $node->{$NPROP_AT_LITERALS}->{$attr_name} );
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub clear_literal_attributes {
	my ($node) = @_;
	$node->{$NPROP_AT_LITERALS} = {};
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub set_literal_attribute {
	my ($node, $attr_name, $attr_value) = @_;
	my $exp_lit_type = $node->expected_literal_attribute_type( $attr_name ); # dies if bad arg
	defined( $attr_value ) or $node->_throw_error_message( 'SSM_N_SET_LIT_AT_NO_ARG_VAL' );

	my $node_type = $node->{$NPROP_NODE_TYPE};

	if( $exp_lit_type eq 'bool' ) {
		if( $attr_value ne '0' and $attr_value ne '1' ) {
			$node->_throw_error_message( 'SSM_N_SET_LIT_AT_INVAL_V_BOOL', 
				{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type, 'VAL' => $attr_value } );
		}

	} elsif( $exp_lit_type eq 'uint' ) {
		if( $attr_value =~ /\D/ or $attr_value < 0 or int($attr_value) ne $attr_value ) {
			# The regexp above should suppress warnings about non-numerical arguments to '<'
			$node->_throw_error_message( 'SSM_N_SET_LIT_AT_INVAL_V_UINT', 
				{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type, 'VAL' => $attr_value } );
		}

	} elsif( $exp_lit_type eq 'sint' ) {
		if( $attr_value =~ /\D/ or int($attr_value) ne $attr_value ) {
			# The regexp above should suppress warnings about non-numerical arguments to '<'
			$node->_throw_error_message( 'SSM_N_SET_LIT_AT_INVAL_V_SINT', 
				{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type, 'VAL' => $attr_value } );
		}

	} else {} # $exp_lit_type eq 'cstr' or 'misc'; no change to value needed

	$node->{$NPROP_AT_LITERALS}->{$attr_name} = $attr_value;
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub set_literal_attributes {
	my ($node, $attrs) = @_;
	defined( $attrs ) or $node->_throw_error_message( 'SSM_N_SET_LIT_ATS_NO_ARGS' );
	unless( ref($attrs) eq 'HASH' ) {
		$node->_throw_error_message( 'SSM_N_SET_LIT_ATS_BAD_ARGS', { 'ARG' => $attrs } );
	}
	foreach my $attr_name (keys %{$attrs}) {
		$node->set_literal_attribute( $attr_name, $attrs->{$attr_name} );
	}
}

######################################################################

sub expected_enumerated_attribute_type {
	my ($node, $attr_name) = @_;
	defined( $attr_name ) or $node->_throw_error_message( 'SSM_N_EXP_ENUM_AT_NO_ARGS' );
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $exp_enum_type = $NODE_TYPES{$node_type}->{$TPI_AT_ENUMS} && 
		$NODE_TYPES{$node_type}->{$TPI_AT_ENUMS}->{$attr_name};
	unless( $exp_enum_type ) {
		$node->_throw_error_message( 'SSM_N_EXP_ENUM_AT_INVAL_NM', 
			{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type } );
	}
	return( $exp_enum_type );
}

sub get_enumerated_attribute {
	my ($node, $attr_name) = @_;
	$node->expected_enumerated_attribute_type( $attr_name ); # dies if bad arg
	return( $node->{$NPROP_AT_ENUMS}->{$attr_name} );
}

sub get_enumerated_attributes {
	return( {%{$_[0]->{$NPROP_AT_ENUMS}}} );
}

sub clear_enumerated_attribute {
	my ($node, $attr_name) = @_;
	$node->expected_enumerated_attribute_type( $attr_name ); # dies if bad arg
	delete( $node->{$NPROP_AT_ENUMS}->{$attr_name} );
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub clear_enumerated_attributes {
	my ($node) = @_;
	$node->{$NPROP_AT_ENUMS} = {};
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub set_enumerated_attribute {
	my ($node, $attr_name, $attr_value) = @_;
	my $exp_enum_type = $node->expected_enumerated_attribute_type( $attr_name ); # dies if bad arg
	defined( $attr_value ) or $node->_throw_error_message( 'SSM_N_SET_ENUM_AT_NO_ARG_VAL' );

	unless( $ENUMERATED_TYPES{$exp_enum_type}->{$attr_value} ) {
		$node->_throw_error_message( 'SSM_N_SET_ENUM_AT_INVAL_V', 
			{ 'NAME' => $attr_name, 'HOSTTYPE' => $node->{$NPROP_NODE_TYPE}, 
			'ENUMTYPE' => $exp_enum_type, 'VAL' => $attr_value } );
	}

	$node->{$NPROP_AT_ENUMS}->{$attr_name} = $attr_value;
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub set_enumerated_attributes {
	my ($node, $attrs) = @_;
	defined( $attrs ) or $node->_throw_error_message( 'SSM_N_SET_ENUM_ATS_NO_ARGS' );
	unless( ref($attrs) eq 'HASH' ) {
		$node->_throw_error_message( 'SSM_N_SET_ENUM_ATS_BAD_ARGS', { 'ARG' => $attrs } );
	}
	foreach my $attr_name (keys %{$attrs}) {
		$node->set_enumerated_attribute( $attr_name, $attrs->{$attr_name} );
	}
}

######################################################################

sub expected_node_ref_attribute_type {
	my ($node, $attr_name) = @_;
	defined( $attr_name ) or $node->_throw_error_message( 'SSM_N_EXP_NREF_AT_NO_ARGS' );
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $exp_node_type = $NODE_TYPES{$node_type}->{$TPI_AT_NREFS} && 
		$NODE_TYPES{$node_type}->{$TPI_AT_NREFS}->{$attr_name};
	unless( $exp_node_type ) {
		$node->_throw_error_message( 'SSM_N_EXP_NREF_AT_INVAL_NM', 
			{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type } );
	}
	return( $exp_node_type );
}

sub get_node_ref_attribute {
	my ($node, $attr_name) = @_;
	$node->expected_node_ref_attribute_type( $attr_name ); # dies if bad arg
	return( $node->{$NPROP_AT_NREFS}->{$attr_name} );
}

sub get_node_ref_attributes {
	return( {%{$_[0]->{$NPROP_AT_NREFS}}} );
}

sub clear_node_ref_attribute {
	my ($node, $attr_name) = @_;
	$node->expected_node_ref_attribute_type( $attr_name ); # dies if bad arg
	$node->_clear_node_ref_attribute( $attr_name );
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub clear_node_ref_attributes {
	my ($node) = @_;
	foreach my $attr_name (sort keys %{$node->{$NPROP_AT_NREFS}}) {
		$node->_clear_node_ref_attribute( $attr_name );
	}
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub _clear_node_ref_attribute {
	my ($node, $attr_name) = @_;
	my $attr_value = $node->{$NPROP_AT_NREFS}->{$attr_name} or return( 1 ); # no-op; attr not set
	if( ref($attr_value) eq ref($node) and $node->{$NPROP_LINKS_RECIP} ) {
		# The attribute value is a Node object, and that Node has linked back, so clear that link.
		my $ra_children_of_parent = $attr_value->{$NPROP_CHILD_NODES};
		foreach my $i (0..$#{$ra_children_of_parent}) {
			if( $ra_children_of_parent->[$i] eq $node ) {
				# remove first instance of $node from it's parent's child list
				splice( @{$ra_children_of_parent}, $i, 1 );
				last;
			}
		}
	}
	delete( $node->{$NPROP_AT_NREFS}->{$attr_name} ); # removes link to parent, if any
}

sub set_node_ref_attribute {
	my ($node, $attr_name, $attr_value) = @_;
	my $exp_node_type = $node->expected_node_ref_attribute_type( $attr_name ); # dies if bad arg
	defined( $attr_value ) or $node->_throw_error_message( 'SSM_N_SET_NREF_AT_NO_ARG_VAL' );

	if( ref($attr_value) eq ref($node) ) {
		# We were given a Node object for a new attribute value.

		unless( $attr_value->{$NPROP_NODE_TYPE} eq $exp_node_type ) {
			$node->_throw_error_message( 'SSM_N_SET_NREF_AT_WRONG_NODE_TYPE', 
				{ 'NAME' => $attr_name, 'HOSTTYPE' => $node->{$NPROP_NODE_TYPE}, 
				'EXPTYPE' => $exp_node_type, 'GIVEN' => $attr_value->{$NPROP_NODE_TYPE} } );
		}

		if( $attr_value->{$NPROP_CONTAINER} and $node->{$NPROP_CONTAINER} ) {
			unless( $attr_value->{$NPROP_CONTAINER} eq $node->{$NPROP_CONTAINER} ) {
				$node->_throw_error_message( 'SSM_N_SET_NREF_AT_DIFF_CONT' );
			}
			# If we get here, both Nodes are in the same Container and can link
		} elsif( $attr_value->{$NPROP_CONTAINER} or $node->{$NPROP_CONTAINER} ) {
			$node->_throw_error_message( 'SSM_N_SET_NREF_AT_ONE_CONT' );
		} elsif( !$attr_value->{$NPROP_NODE_ID} ) {
			# both Nodes are not in Containers, and $attr_value has no Node Id
			$node->_throw_error_message( 'SSM_N_SET_NREF_AT_MISS_NID' );
		} else {
			# both Nodes are not in Containers, and $attr_value has Node Id, so can link
			$attr_value = $attr_value->{$NPROP_NODE_ID};
		} 

	} else {
		# We may have been given a Node id for a new attribute value.
		if( $attr_value =~ /\D/ or $attr_value < 1 or int($attr_value) ne $attr_value ) {
			# The regexp above should suppress warnings about non-numerical arguments to '<'
			$node->_throw_error_message( 'SSM_N_SET_NREF_AT_BAD_ARG_VAL', { 'ARG' => $attr_value } );
		}

		if( my $container = $node->{$NPROP_CONTAINER} ) {
			$attr_value = $container->{$CPROP_ALL_NODES}->{$exp_node_type}->{$attr_value};
			unless( $attr_value ) {
				$node->_throw_error_message( 'SSM_N_SET_NREF_AT_NONEX_NID', 
					{ 'ARG' => $attr_value, 'EXPTYPE' => $exp_node_type } );
			}
		}
	}

	if( ref($attr_value) eq ref($node) and !$attr_value->{$NPROP_LINKS_RECIP} ) {
		$node->_throw_error_message( 'SSM_N_SET_NREF_AT_RECIP_LINKS' );
	}

	if( defined( $node->{$NPROP_AT_NREFS}->{$attr_name} ) and
			$attr_value eq $node->{$NPROP_AT_NREFS}->{$attr_name} ) {
		return( 1 ); # no-op; new attribute value same as old
	}

	if( ref($attr_value) eq ref($node) ) {
		# Attempt is to link two Nodes in the same Container; it would be okay, except 
		# that we still have to check for circular primary parent Node references.
		my $parent_node = $attr_value;
		while( $parent_node = $parent_node->get_parent_node() ) {
			if( $parent_node eq $node ) {
				$node->_throw_error_message( 'SSM_N_SET_NREF_AT_CIRC_REF' );
			}
		}
	}

	$node->_clear_node_ref_attribute( $attr_name ); # clears any existing link through this attribute
	$node->{$NPROP_AT_NREFS}->{$attr_name} = $attr_value;
	if( ref($attr_value) eq ref($node) and $node->{$NPROP_LINKS_RECIP} ) {
		# The attribute value is a Node object, and that Node should link back now, so do it.
		push( @{$attr_value->{$NPROP_CHILD_NODES}}, $node );
	}
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub set_node_ref_attributes {
	my ($node, $attrs) = @_;
	defined( $attrs ) or $node->_throw_error_message( 'SSM_N_SET_NREF_ATS_NO_ARGS' );
	unless( ref($attrs) eq 'HASH' ) {
		$node->_throw_error_message( 'SSM_N_SET_NREF_ATS_BAD_ARGS', { 'ARG' => $attrs } );
	}
	foreach my $attr_name (sort keys %{$attrs}) {
		$node->set_node_ref_attribute( $attr_name, $attrs->{$attr_name} );
	}
}

######################################################################

sub expected_attribute_major_type {
	my ($node, $attr_name) = @_;
	defined( $attr_name ) or $node->_throw_error_message( 'SSM_N_EXP_AT_MT_NO_ARGS' );
	my $node_type = $node->get_node_type();
	my $namt = $node->major_type_of_node_type_attribute( $node_type, $attr_name );
	unless( $namt ) {
		$node->_throw_error_message( 'SSM_N_EXP_AT_MT_INVAL_NM', 
			{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type } );
	}
	return( $namt );
}

sub get_attribute {
	my ($node, $attr_name) = @_;
	my $namt = $node->expected_attribute_major_type( $attr_name ); # dies if bad arg
	$namt eq $NAMT_ID and return( $node->get_node_id() );
	$namt eq $NAMT_LITERAL and return( $node->get_literal_attribute( $attr_name ) );
	$namt eq $NAMT_ENUM and return( $node->get_enumerated_attribute( $attr_name ) );
	$namt eq $NAMT_NODE and return( $node->get_node_ref_attribute( $attr_name ) );
	# We should never get here.
}

sub get_attributes {
	my ($node) = @_;
	return( {
		$ATTR_ID => $node->get_node_id(),
		%{$node->get_literal_attributes()},
		%{$node->get_enumerated_attributes()},
		%{$node->get_node_ref_attributes()},
	} );
}

sub clear_attribute {
	my ($node, $attr_name) = @_;
	my $namt = $node->expected_attribute_major_type( $attr_name ); # dies if bad arg
	$namt eq $NAMT_ID and return( $node->clear_node_id() );
	$namt eq $NAMT_LITERAL and return( $node->clear_literal_attribute( $attr_name ) );
	$namt eq $NAMT_ENUM and return( $node->clear_enumerated_attribute( $attr_name ) );
	$namt eq $NAMT_NODE and return( $node->clear_node_ref_attribute( $attr_name ) );
	# We should never get here.
}

sub clear_attributes {
	my ($node) = @_;
	$node->clear_node_id();
	$node->clear_literal_attributes();
	$node->clear_enumerated_attributes();
	$node->clear_node_ref_attributes();
}

sub set_attribute {
	my ($node, $attr_name, $attr_value) = @_;
	my $namt = $node->expected_attribute_major_type( $attr_name ); # dies if bad arg
	$namt eq $NAMT_ID and return( $node->set_node_id( $attr_value ) );
	$namt eq $NAMT_LITERAL and return( $node->set_literal_attribute( $attr_name, $attr_value ) );
	$namt eq $NAMT_ENUM and return( $node->set_enumerated_attribute( $attr_name, $attr_value ) );
	$namt eq $NAMT_NODE and return( $node->set_node_ref_attribute( $attr_name, $attr_value ) );
	# We should never get here.
}

sub set_attributes {
	my ($node, $attrs) = @_;
	defined( $attrs ) or $node->_throw_error_message( 'SSM_N_SET_ATS_NO_ARGS' );
	unless( ref($attrs) eq 'HASH' ) {
		$node->_throw_error_message( 'SSM_N_SET_ATS_BAD_ARGS', { 'ARG' => $attrs } );
	}
	foreach my $attr_name (sort keys %{$attrs}) {
		my $attr_value = $attrs->{$attr_name};
		my $namt = $node->expected_attribute_major_type( $attr_name ); # dies if bad arg
		if( $namt eq $NAMT_ID ) {
			$node->set_node_id( $attr_value );
			next;
		}
		if( $namt eq $NAMT_LITERAL ) {
			$node->set_literal_attribute( $attr_name, $attr_value );
			next;
		}
		if( $namt eq $NAMT_ENUM ) {
			$node->set_enumerated_attribute( $attr_name, $attr_value );
			next;
		}
		if( $namt eq $NAMT_NODE ) {
			$node->set_node_ref_attribute( $attr_name, $attr_value );
			next;
		}
		# We should never get here.
	}
}

######################################################################

sub get_parent_node_attribute_name {
	return( $_[0]->{$NPROP_P_NODE_ATNM} );
}

sub get_parent_node {
	my ($node) = @_;
	if( $node->{$NPROP_P_NODE_ATNM} and $node->{$NPROP_CONTAINER} ) {
		# Note that the associated AT_NREFS property may not be valued right now.
		# This code may be changed later to return a Node id when not in a container.
		return( $node->{$NPROP_AT_NREFS}->{$node->{$NPROP_P_NODE_ATNM}} );
	}
}

sub clear_parent_node_attribute_name {
	my ($node) = @_;
	$node->{$NPROP_P_NODE_ATNM} = undef;
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

sub set_parent_node_attribute_name {
	my ($node, $attr_name) = @_;
	defined( $attr_name ) or $node->_throw_error_message( 'SSM_N_SET_P_NODE_ATNM_NO_ARGS' );
	my $node_type = $node->{$NPROP_NODE_TYPE};
	unless( $NODE_TYPES{$node_type}->{$TPI_P_NODE_ATNMS} and 
			grep { $_ eq $attr_name } @{$NODE_TYPES{$node_type}->{$TPI_P_NODE_ATNMS}} ) {
		$node->_throw_error_message( 'SSM_N_SET_P_NODE_ATNM_INVAL_NM', 
			{ 'NAME' => $attr_name, 'HOSTTYPE' => $node_type } );
	}
	if( defined( $node->{$NPROP_P_NODE_ATNM} ) and
			$attr_name eq $node->{$NPROP_P_NODE_ATNM} ) {
		return( 1 ); # no-op; new primary parent name same as old
	}
	if( $node->{$NPROP_CONTAINER} and $node->{$NPROP_AT_NREFS}->{$attr_name} ) {
		# Attempt is to set an already-linked parent Node as this current Node's 
		# primary parent; it would be okay, except we have to make sure the change 
		# won't create a circular primary parent reference chain.
		my $parent_node = $node->{$NPROP_AT_NREFS}->{$attr_name};
		while( $parent_node = $parent_node->get_parent_node() ) {
			if( $parent_node eq $node ) {
				$node->_throw_error_message( 'SSM_N_SET_P_NODE_ATNM_CIRC_REF', 
					{ 'NAME' => $attr_name } );
			}
		}
	}
	$node->{$NPROP_P_NODE_ATNM} = $attr_name;
	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node was changed.
	}
}

######################################################################

sub estimate_parent_node_attribute_name {
	# This function tries to find a way to make its argument Node a primary parent of 
	# the current Node; it returns the first appropriate node attribute name which 
	# takes a Node of the same node type of the argument.
	my ($node, $new_parent, $only_not_valued) = @_;
	defined( $new_parent ) or $node->_throw_error_message( 'SSM_N_EST_P_NODE_ATNM_NO_ARGS' );
	unless( ref($new_parent) eq ref($node) ) {
		$node->_throw_error_message( 'SSM_N_EST_P_NODE_ATNM_BAD_ARG', { 'ARG' => $new_parent } );
	}
	my $parent_node_type = $new_parent->{$NPROP_NODE_TYPE};
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $p_node_atnms = $NODE_TYPES{$node_type}->{$TPI_P_NODE_ATNMS} or return( undef ); # can't have any parent
	my $exp_at_nodes = $NODE_TYPES{$node_type}->{$TPI_AT_NREFS}; # assume exists, as prev does
	my $at_nodes = $node->{$NPROP_AT_NREFS};
	foreach my $attr_name (@{$p_node_atnms}) {
		my $exp_at_node = $exp_at_nodes->{$attr_name};
		if( $parent_node_type eq $exp_at_node ) {
			# If we get here, we found a primary parent attribute which is of the right type.
			$only_not_valued and $at_nodes->{$attr_name} and next; # can't use when has value; keep looking
			return( $attr_name ); # no value set or may overwrite it
		}
	}
	return( undef ); # given Node wrong type or competitor for primary parent of current Node
}

######################################################################

sub get_container {
	return( $_[0]->{$NPROP_CONTAINER} );
}

sub put_in_container {
	my ($node, $new_container) = @_;
	defined( $new_container ) or $node->_throw_error_message( 'SSM_N_PI_CONT_NO_ARGS' );

	unless( ref($new_container) and UNIVERSAL::isa( $new_container, 'SQL::SyntaxModel::Container' ) ) {
		$node->_throw_error_message( 'SSM_N_PI_CONT_BAD_ARG', { 'ARG' => $new_container } );
	}

	my $node_id = $node->{$NPROP_NODE_ID};
	unless( $node_id ) {
		$node->_throw_error_message( 'SSM_N_PI_CONT_NO_NODE_ID' );
	}

	if( $node->{$NPROP_CONTAINER} ) {
		if( $new_container eq $node->{$NPROP_CONTAINER} ) {
			return( 1 ); # no-op; new container same as old
		}
		$node->_throw_error_message( 'SSM_N_PI_CONT_HAVE_ALREADY' );
	}
	my $node_type = $node->{$NPROP_NODE_TYPE};

	if( $new_container->{$CPROP_ALL_NODES}->{$node_type}->{$node_id} ) {
		$node->_throw_error_message( 'SSM_N_PI_CONT_DUPL_ID', 
			{ 'ID' => $node_id, 'TYPE' => $node_type } );
	}

	# Note: No recursion tests are necessary in put_in_container(); any existing Node 
	# that the newly added Node would link to can not already be the new Node's direct 
	# or indirect child, since Nodes in Containers can't reference Nodes that aren't.

	my $tpi_at_nodes = $NODE_TYPES{$node_type}->{$TPI_AT_NREFS};
	my $rh_at_nodes_nids = $node->{$NPROP_AT_NREFS}; # all values should be node ids now
	my $rh_cnl_bt = $new_container->{$CPROP_ALL_NODES};

	my %at_nodes_refs = (); # values put in here will be actual references
	foreach my $at_nodes_atnm (keys %{$rh_at_nodes_nids}) {
		# We need to make sure that when an attribute value is cleared, its key is deleted
		# Note that if $tpi_at_nodes is undefined, expect that this foreach loop will not run
		my $at_nodes_nid = $rh_at_nodes_nids->{$at_nodes_atnm};
		my $at_node_type = $tpi_at_nodes->{$at_nodes_atnm};
		my $at_nodes_ref = $rh_cnl_bt->{$at_node_type}->{$at_nodes_nid};
		unless( $at_nodes_ref ) {
			$node->_throw_error_message( 'SSM_N_PI_CONT_NONEX_AT_NREF', 
				{ 'ATNM' => $at_nodes_atnm, 'TYPE' => $at_node_type, 'ID' => $at_nodes_nid } );
		}
		$at_nodes_refs{$at_nodes_atnm} = $at_nodes_ref;
	}
	$node->{$NPROP_CONTAINER} = $new_container;
	$node->{$NPROP_AT_NREFS} = \%at_nodes_refs;
	$rh_cnl_bt->{$node_type}->{$node_id} = $node;
	# We don't get referenced nodes to link back here; caller requests that separately

	# Now adjust our "next free node id" counter if appropriate
	my $rh_cnfni = $node->{$NPROP_CONTAINER}->{$CPROP_NEXT_FREE_NIDS};
	if( $node_id >= $rh_cnfni->{$node_type} ) {
		$rh_cnfni->{$node_type} = 1 + $node_id;
	}
}

sub take_from_container {
	my ($node) = @_;
	my $container = $node->{$NPROP_CONTAINER} or return( 1 ); # no-op; node is already not in a container

	if( $node->{$NPROP_LINKS_RECIP} ) {
		$node->_throw_error_message( 'SSM_N_TF_CONT_RECIP_LINKS' );
	}

	my $node_id = $node->{$NPROP_NODE_ID};
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $rh_at_nodes_refs = $node->{$NPROP_AT_NREFS};

	my %at_nodes_nids = (); # values put in here will be node id numbers
	foreach my $at_nodes_atnm (keys %{$rh_at_nodes_refs}) {
		# We need to make sure that when an attribute value is cleared, its key is deleted
		$at_nodes_nids{$at_nodes_atnm} = $rh_at_nodes_refs->{$at_nodes_atnm}->{$NPROP_NODE_ID};
	}

	delete( $node->{$NPROP_CONTAINER}->{$CPROP_ALL_NODES}->{$node_type}->{$node_id} );
	$node->{$NPROP_AT_NREFS} = \%at_nodes_nids;
	$node->{$NPROP_CONTAINER} = undef;
}

######################################################################

sub are_reciprocal_links {
	# A true value just means any links we may make will reciprocate;
	# we may not actually have any links yet.
	return( $_[0]->{$NPROP_LINKS_RECIP} );
}

sub add_reciprocal_links {
	my ($node) = @_;
	$node->{$NPROP_LINKS_RECIP} and return( 1 ); # no-op; links are already reciprocated

	my $container = $node->{$NPROP_CONTAINER};
	unless( $container ) {
		$node->_throw_error_message( 'SSM_N_ADD_RL_NO_NODE_ID' );
	}

	my $node_type = $node->{$NPROP_NODE_TYPE};
	if( my $p_pseudonode = $NODE_TYPES{$node_type}->{$TPI_P_PSEUDONODE} ) {
		push( @{$container->{$CPROP_PSEUDONODES}->{$p_pseudonode}}, $node );
	}

	foreach my $attr_value (values %{$node->{$NPROP_AT_NREFS}}) {
		push( @{$attr_value->{$NPROP_CHILD_NODES}}, $node );
	}

	$node->{$NPROP_LINKS_RECIP} = 1;
	$container->{$CPROP_DEF_CON_TESTED} = 0; # A Node has become "Well Known".
}

sub remove_reciprocal_links {
	my ($node) = @_;
	$node->{$NPROP_LINKS_RECIP} or return( 1 ); # no-op; links are already not reciprocated

	if( @{$node->{$NPROP_CHILD_NODES}} > 0 ) {
		$node->_throw_error_message( 'SSM_N_REM_RL_HAS_CHILD' );
	}

	my $node_type = $node->{$NPROP_NODE_TYPE};
	if( my $p_pseudonode = $NODE_TYPES{$node_type}->{$TPI_P_PSEUDONODE} ) {
		my $container = $node->{$NPROP_CONTAINER};
		my $siblings = $container->{$CPROP_PSEUDONODES}->{$p_pseudonode};
		@{$siblings} = grep { $_ ne $node } @{$siblings}; # remove all occurances
	}

	foreach my $attr_value (@{$node->{$NPROP_AT_NREFS}}) {
		my $siblings = $attr_value->{$NPROP_CHILD_NODES};
		@{$siblings} = grep { $_ ne $node } @{$siblings}; # remove all occurances
	}

	$node->{$NPROP_LINKS_RECIP} = 0;
	$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # A "Well Known" Node is gone.
		# Turn on tests because this Node's absence affects *other* Well Known Nodes.
}

######################################################################

sub move_before_sibling {
	my ($node, $sibling, $parent) = @_;
	my $p_pseudonode = $NODE_TYPES{$node->{$NPROP_NODE_TYPE}}->{$TPI_P_PSEUDONODE};

	# First make sure we have 3 actual Nodes that are all "Well Known" and in the same Container.

	$node->{$NPROP_LINKS_RECIP} or $node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_NO_RL' );

	defined( $sibling ) or $node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_NO_S_ARG' );
	unless( ref($sibling) eq ref($node) ) {
		$node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_BAD_S_ARG', { 'ARG' => $sibling } );
	}
	$sibling->{$NPROP_LINKS_RECIP} or $node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_S_NO_RL' );
	unless( $sibling->{$NPROP_CONTAINER} eq $node->{$NPROP_CONTAINER} ) {
		$node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_S_DIFF_CONT' );
	}

	if( defined( $parent ) ) {
		unless( ref($parent) eq ref($node) ) {
			$node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_BAD_P_ARG', { 'ARG' => $parent } );
		}
		unless( $parent->{$NPROP_CONTAINER} and $parent->{$NPROP_CONTAINER} eq $node->{$NPROP_CONTAINER} ) {
			$node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_P_DIFF_CONT' );
		}
	} else {
		unless( $node->{$NPROP_P_NODE_ATNM} and 
				$parent = $node->{$NPROP_AT_NREFS}->{$node->{$NPROP_P_NODE_ATNM}} ) {
			$p_pseudonode or $node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_NO_P_ARG_OR_PP_OR_PS' );
		}
	}

	# Now get the Node list we're going to search through.

	my $ra_search_list = $parent ? $parent->{$NPROP_CHILD_NODES} : 
		$node->{$NPROP_CONTAINER}->{$CPROP_PSEUDONODES}->{$p_pseudonode};

	# Now confirm the given Nodes are our parent and sibling.
	# For efficiency we also prepare to reorder the Nodes at the same time.

	my @curr_node_refs = ();
	my @sib_node_refs = ();
	my @refs_before_both = ();
	my @refs_after_both = ();

	my $others_go_before = 1;
	foreach my $child (@{$ra_search_list}) {
		if( $child eq $node ) {
			push( @curr_node_refs, $child );
		} elsif( $child eq $sibling ) {
			push( @sib_node_refs, $child );
			$others_go_before = 0;
		} elsif( $others_go_before ) {
			push( @refs_before_both, $child );
		} else {
			push( @refs_after_both, $child );
		}
	}

	scalar( @curr_node_refs ) or $node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_P_NOT_P' );
	scalar( @sib_node_refs ) or $node->_throw_error_message( 'SSM_N_MOVE_PRE_SIB_S_NOT_S' );

	# Everything checks out, so now we perform the reordering.

	@{$ra_search_list} = (@refs_before_both, @curr_node_refs, @sib_node_refs, @refs_after_both);
	$node->{$NPROP_CONTAINER}->{$CPROP_DEF_CON_TESTED} = 0; # "Well Known" Node relation chg.
}

######################################################################

sub get_child_nodes {
	my ($node, $node_type) = @_;
	if( defined( $node_type ) ) {
		unless( $NODE_TYPES{$node_type} ) {
			$node->_throw_error_message( 'SSM_N_GET_CH_NODES_BAD_TYPE', { 'TYPE' => $node_type } );
		}
		return( [grep { $_->{$NPROP_NODE_TYPE} eq $node_type } @{$node->{$NPROP_CHILD_NODES}}] );
	} else {
		return( [@{$node->{$NPROP_CHILD_NODES}}] );
	}
}

sub add_child_node {
	my ($node, $new_child) = @_;
	defined( $new_child ) or $node->_throw_error_message( 'SSM_N_ADD_CH_NODE_NO_ARGS' );
	unless( ref($new_child) eq ref($node) ) {
		$node->_throw_error_message( 'SSM_N_ADD_CH_NODE_BAD_ARG', { 'ARG' => $new_child } );
	}
	my $est_attr_name = $new_child->estimate_parent_node_attribute_name( $node );
	unless( $est_attr_name ) {
		$node->_throw_error_message( 'SSM_N_ADD_CH_NODE_NO_EST' );
	}
	$new_child->set_node_ref_attribute( $est_attr_name, $node ); # will die if not same Container
		# will also die if the change would result in a circular reference
	$new_child->set_parent_node_attribute_name( $est_attr_name );
}

sub add_child_nodes {
	my ($node, $list) = @_;
	$list or return( undef );
	unless( ref($list) eq 'ARRAY' ) {
		$list = [ $list ];
	}
	foreach my $element (@{$list}) {
		$node->add_child_node( $element );
	}
}

######################################################################

sub test_deferrable_constraints {
	my ($node) = @_;
	my $node_type = $node->{$NPROP_NODE_TYPE};
	my $node_id = $node->{$NPROP_NODE_ID};
	my $type_info = $NODE_TYPES{$node_type};

	# 1: Now test constraints associated with Node-type details given in each 
	# "Attribute List" section of Language.pod.

	# 1.1: Assert that the NODE_ID attribute is set.
	unless( defined( $node_id ) ) {
		# This can only possibly fail at deferred-constraint assertion time with "Alone" Nodes; 
		# it is always-enforced for "At Home" and "Well Known" Nodes.
		$node->_throw_error_message( 'SSM_N_TEDC_NID_VAL_NO_SET', { 'HOSTTYPE' => $node_type } );
	}

	# 1.2: Assert that a Node which can have a Node primary-parent does in fact have one.
	if( !$type_info->{$TPI_P_PSEUDONODE} and !$node->{$NPROP_P_NODE_ATNM} ) {
		$node->_throw_error_message( 'SSM_N_TEDC_P_NODE_ATNM_NOT_SET', 
			{ 'HOSTTYPE' => $node_type, 'ID' => $node_id } );
	}

	# 1.3: Assert that exactly one primary parent ("PP") Node attribute is set.
	if( my $p_node_atnms = $NODE_TYPES{$node_type}->{$TPI_P_NODE_ATNMS} ) {
		my @valued_candidates = ();
		foreach my $attr_name (@{$p_node_atnms}) {
			if( defined( $node->{$NPROP_AT_NREFS}->{$attr_name} ) ) {
				push( @valued_candidates, $attr_name );
			}
		}
		if( scalar( @valued_candidates ) > 1 ) {
			$node->_throw_error_message( 'SSM_N_TEDC_PP_TOO_MANY_SET', 
				{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 
				'NUMVALS' => scalar( @valued_candidates ), 'ATNMS' => "@valued_candidates" } );
		}
		if( scalar( @valued_candidates ) == 0 ) {
			my @possible_candidates = @{$p_node_atnms};
			$node->_throw_error_message( 'SSM_N_TEDC_PP_ZERO_SET', 
				{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'ATNMS' => "@possible_candidates" } );
		}
	}

	# 1.4: Assert that any always-mandatory ("MA") attributes are set.
	if( my $mand_attrs = $type_info->{$TPI_MA_ATTRS} ) {
		my ($lits, $enums, $nrefs) = @{$mand_attrs};
		foreach my $attr_name (@{$lits}) {
			unless( defined( $node->{$NPROP_AT_LITERALS}->{$attr_name} ) ) {
				$node->_throw_error_message( 'SSM_N_TEDC_MA_LIT_VAL_NO_SET', 
					{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'ATNM' => $attr_name } );
			}
		}
		foreach my $attr_name (@{$enums}) {
			unless( defined( $node->{$NPROP_AT_ENUMS}->{$attr_name} ) ) {
				$node->_throw_error_message( 'SSM_N_TEDC_MA_ENUM_VAL_NO_SET', 
					{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'ATNM' => $attr_name } );
			}
		}
		foreach my $attr_name (@{$nrefs}) {
			unless( defined( $node->{$NPROP_AT_NREFS}->{$attr_name} ) ) {
				$node->_throw_error_message( 'SSM_N_TEDC_MA_NREF_VAL_NO_SET', 
					{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'ATNM' => $attr_name } );
			}
		}
	}

	# 2: Now test constraints associated with Node-type details given in each 
	# "Exclusive Attribute Groups List" section of Language.pod.

	if( my $mutex_atgps = $NODE_TYPES{$node_type}->{$TPI_MUTEX_ATGPS} ) {
		foreach my $mutex_atgp (@{$mutex_atgps}) {
			my ($mutex_name, $lits, $enums, $nrefs, $is_mandatory) = @{$mutex_atgp};
			my @valued_candidates = ();
			foreach my $attr_name (@{$lits}) {
				if( defined( $node->{$NPROP_AT_LITERALS}->{$attr_name} ) ) {
					push( @valued_candidates, $attr_name );
				}
			}
			foreach my $attr_name (@{$enums}) {
				if( defined( $node->{$NPROP_AT_ENUMS}->{$attr_name} ) ) {
					push( @valued_candidates, $attr_name );
				}
			}
			foreach my $attr_name (@{$nrefs}) {
				if( defined( $node->{$NPROP_AT_NREFS}->{$attr_name} ) ) {
					push( @valued_candidates, $attr_name );
				}
			}
			if( scalar( @valued_candidates ) > 1 ) {
				$node->_throw_error_message( 'SSM_N_TEDC_MUTEX_TOO_MANY_SET', 
					{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 
					'NUMVALS' => scalar( @valued_candidates ), 
					'ATNMS' => "@valued_candidates", 'MUTEX' => $mutex_name } );
			}
			if( scalar( @valued_candidates ) == 0 ) {
				if( $is_mandatory ) {
					my @possible_candidates = (@{$lits}, @{$enums}, @{$nrefs});
					$node->_throw_error_message( 'SSM_N_TEDC_MUTEX_ZERO_SET', 
						{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 
						'ATNMS' => "@possible_candidates", 'MUTEX' => $mutex_name } );
				}
			}
		}
	}

	# 3: Now test constraints associated with Node-type details given in each 
	# "Local Attribute Dependencies List" section of Language.pod.

	if( my $local_atdps_list = $NODE_TYPES{$node_type}->{$TPI_LOCAL_ATDPS} ) {
		foreach my $local_atdps_item (@{$local_atdps_list}) {
			my ($dep_on_lit_nm, $dep_on_enum_nm, $dep_on_nref_nm, $dependencies) = @{$local_atdps_item};
			foreach my $dependency (@{$dependencies}) {
				my ($lits, $enums, $nrefs, $dep_on_enum_vals, $is_mandatory) = @{$dependency};
				my @valued_dependents = ();
				foreach my $attr_name (@{$lits}) {
					if( defined( $node->{$NPROP_AT_LITERALS}->{$attr_name} ) ) {
						push( @valued_dependents, $attr_name );
					}
				}
				foreach my $attr_name (@{$enums}) {
					if( defined( $node->{$NPROP_AT_ENUMS}->{$attr_name} ) ) {
						push( @valued_dependents, $attr_name );
					}
				}
				foreach my $attr_name (@{$nrefs}) {
					if( defined( $node->{$NPROP_AT_NREFS}->{$attr_name} ) ) {
						push( @valued_dependents, $attr_name );
					}
				}
				if( $dep_on_lit_nm ) {
					my $dep_on_attr_val = $node->{$NPROP_AT_LITERALS}->{$dep_on_lit_nm};
					if( !defined( $dep_on_attr_val ) ) {
						# The dependency is undef/null, so all dependents must be undef/null.
						if( scalar( @valued_dependents ) > 0 ) {
							$node->_throw_error_message( 'SSM_N_TEDC_LATDP_DEP_ON_IS_NULL', 
								{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'DEP_ON' => $dep_on_lit_nm,
								'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
						}
					} else {
						# The dependency is valued, so a dependent may be set.
						# SHORT CUT: We know that with all of our existing config data, 
						# @valued_dependents has only 0..1 elements, and that none are mandatory.
						# So no more tests to do.  We will put code here later if either fact changes.
					}
					# If we get here, the tests have passed concerning this $dependency.
					next;
				}
				if( $dep_on_enum_nm ) {
					my $dep_on_attr_val = $node->{$NPROP_AT_ENUMS}->{$dep_on_enum_nm};
					if( !defined( $dep_on_attr_val ) ) {
						# The dependency is undef/null, so all dependents must be undef/null.
						if( scalar( @valued_dependents ) > 0 ) {
							$node->_throw_error_message( 'SSM_N_TEDC_LATDP_DEP_ON_IS_NULL', 
								{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'DEP_ON' => $dep_on_enum_nm,
								'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
						}
						# If we get here, the tests have passed concerning this $dependency.
					} elsif( !scalar( grep { $_ eq $dep_on_attr_val } @{$dep_on_enum_vals} ) ) {
						# The dependency has the wrong value for these dependents; the latter must be undef/null.
						if( scalar( @valued_dependents ) > 0 ) {
							$node->_throw_error_message( 'SSM_N_TEDC_LATDP_DEP_ON_HAS_WRONG_VAL', 
								{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'DEP_ON' => $dep_on_enum_nm,
								'DEP_ON_VAL' => $dep_on_attr_val, 
								'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
						}
						# If we get here, the tests have passed concerning this $dependency.
					} else {
						# The dependency has the right value for these dependents; one of them may be set.
						if( scalar( @valued_dependents ) > 1 ) {
							$node->_throw_error_message( 'SSM_N_TEDC_LATDP_TOO_MANY_SET', 
								{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'DEP_ON' => $dep_on_enum_nm,
								'DEP_ON_VAL' => $dep_on_attr_val, 
								'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
						}
						if( scalar( @valued_dependents ) == 0 ) {
							if( $is_mandatory ) {
								my @possible_candidates = (@{$lits}, @{$enums}, @{$nrefs});
								$node->_throw_error_message( 'SSM_N_TEDC_LATDP_ZERO_SET', 
									{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'DEP_ON' => $dep_on_enum_nm,
								'DEP_ON_VAL' => $dep_on_attr_val, 'ATNMS' => "@possible_candidates" } );
							}
						}
						# If we get here, the tests have passed concerning this $dependency.
					}
					next;
				}
				if( $dep_on_nref_nm ) {
					my $dep_on_attr_val = $node->{$NPROP_AT_NREFS}->{$dep_on_nref_nm};
					if( !defined( $dep_on_attr_val ) ) {
						# The dependency is undef/null, so all dependents must be undef/null.
						if( scalar( @valued_dependents ) > 0 ) {
							$node->_throw_error_message( 'SSM_N_TEDC_LATDP_DEP_ON_IS_NULL', 
								{ 'HOSTTYPE' => $node_type, 'ID' => $node_id, 'DEP_ON' => $dep_on_nref_nm,
								'NUMVALS' => scalar( @valued_dependents ), 'ATNMS' => "@valued_dependents" } );
						}
					} else {
						# The dependency is valued, so a dependent may be set.
						# SHORT CUT: We know that with all of our existing config data, 
						# @valued_dependents has only 0..1 elements, and that none are mandatory.
						# So no more tests to do.  We will put code here later if either fact changes.
					}
					# If we get here, the tests have passed concerning this $dependency.
					next;
				}
			}
		}
	}

	# This is the end of the tests that can be performed on "Alone" Nodes.

	my $container = $node->{$NPROP_CONTAINER};
	unless( $container ) {
		return( 1 ); # "Alone" Nodes quit now.
	}

	# If we get here, Node is "At Home" or "Well Known".
	# However, only "Well Known" Nodes would get invoked by Container.test_deferrable_constraints(); 
	# "At Home" Nodes only get this far when Node.test_deferrable_constraints() invoked externally.

	# TODO: Tests that examine a Node's correctness based on attributes of related Nodes.
}

######################################################################

sub get_all_properties {
	return( $_[0]->_get_all_properties() );
}

sub _get_all_properties {
	my ($node) = @_;
	my %dump = ();

	$dump{$DBG_GAP_NODE_TYPE} = $node->{$NPROP_NODE_TYPE};

	my $at_nodes_in = $node->{$NPROP_AT_NREFS};
	$dump{$DBG_GAP_ATTRS} = {
		$ATTR_ID => $node->{$NPROP_NODE_ID},
		%{$node->{$NPROP_AT_LITERALS}},
		%{$node->{$NPROP_AT_ENUMS}},
		(map { ( $_ => $at_nodes_in->{$_}->{$NPROP_NODE_ID} ) } keys %{$at_nodes_in}),
	};

	my @children_out = ();
	my %children_were_output = ();
	foreach my $child (@{$node->{$NPROP_CHILD_NODES}}) {
		if( my $child_p_node_atnm = $child->{$NPROP_P_NODE_ATNM} ) {
			if( my $child_main_parent = $child->{$NPROP_AT_NREFS}->{$child_p_node_atnm} ) {
				if( $child_main_parent eq $node ) {
					# Only output child if we are its primary parent, not simply any parent.
					unless( $children_were_output{$child} ) {
						# Only output child once; a child may link to same parent multiple times.
						push( @children_out, $child->_get_all_properties() );
						$children_were_output{$child} = 1;
					}
				}
			}
		}
	}
	$dump{$DBG_GAP_CHILDREN} = \@children_out;

	return( \%dump );
}

sub get_all_properties_as_perl_str {
	return( $_[0]->_serialize_as_perl( $_[1], $_[0]->_get_all_properties() ) );
}

sub get_all_properties_as_xml_str {
	return( $_[0]->_serialize_as_xml( $_[1], $_[0]->_get_all_properties() ) );
}

######################################################################
######################################################################

1;
__END__

=head1 SYNOPSIS

=head2 Model-Building Perl Code Examples

This module's native API is highly verbose / detailed and so a realistically
complete example of its use would be too large to show here.  (In fact, most
real uses of the module would involve user-picked wrapper functions that aren't
included.)  However, here are a few example usage lines:

	use SQL::SyntaxModel;

	eval {
		my $model = SQL::SyntaxModel->new_container();

		# ... add a few Nodes

		# Create user-defined data type domain that our database record primary keys are:
		my $dom_entity_id = SQL::SyntaxModel->new_node( 'domain' );
		$dom_entity_id->set_node_id( 1 );
		$dom_entity_id->put_in_container( $model );
		$dom_entity_id->add_reciprocal_links();
		$dom_entity_id->set_node_ref_attribute( 'schema', $schema );
		$dom_entity_id->set_parent_node_attribute_name( 'schema' );
		$dom_entity_id->set_literal_attribute( 'name', 'entity_id' );
		$dom_entity_id->set_enumerated_attribute( 'base_type', 'NUM_INT' );
		$dom_entity_id->set_literal_attribute( 'num_precision', 9 );

		# Define the table that holds our data:
		my $tb_person = $pp_node->new_node( 'table' );
		$tb_person->set_node_id( 1 );
		$tb_person->put_in_container( $model );
		$tb_person->add_reciprocal_links();
		$tb_person->set_node_ref_attribute( 'schema', $schema );
		$tb_person->set_parent_node_attribute_name( 'schema' );
		$tb_person->set_literal_attribute( 'name', 'person' );

		# Define the 'person id' column of that table:
		my $tbc_person_id = $pp_node->new_node( 'table_col' );
		$tbc_person_id->set_node_id( 1 );
		$tbc_person_id->put_in_container( $model );
		$tbc_person_id->add_reciprocal_links();
		$tbc_person_id->set_node_ref_attribute( 'table', $tb_person );
		$tbc_person_id->set_parent_node_attribute_name( 'table' );
		$tbc_person_id->set_literal_attribute( 'name', 'person_id' );
		$tbc_person_id->set_node_ref_attribute( 'domain', $dom_entity_id );
		$tbc_person_id->set_literal_attribute( 'mandatory', 1 );
		$tbc_person_id->set_literal_attribute( 'default_val', 1 );
		$tbc_person_id->set_literal_attribute( 'auto_inc', 1 );

		# ... add a lot more Nodes

		# Now check that we didn't omit something important:
		$model->test_deferrable_constraints();

		# Now serialize all our Nodes to see if we stored what we expected:
		print $model->get_all_properties_as_xml_str();

		# Now explicitly destroy our Container so we don't leak memory:
		$model->destroy();
	};

	if( my $message = $@ ) {
		my $translator = Locale::KeyedText->new_translator( ['SQL::SyntaxModel::L::'], ['en'] );
		my $user_text = $translator->translate_message( $message );
		unless( $user_text ) {
			$user_text = ref($message) ? "internal error: can't find user text for a message: ".
				$message->as_string()." ".$translator->as_string() : $message;
		}
		print "SOMETHING'S WRONG: $user_text\n";
	}

The above code sample is taken and slightly altered from a longer set of code
in this module's test script/module: 't/SQL_SyntaxModel.t' and
'lib/t_SQL_SyntaxModel.pm'.  Even that code is an incomplete sample, but it 
also demonstrates the use of a couple simple wrapper functions.

=head2 A Complete Example Model Serialized to XML

This is a serialization of the model that the test code makes, which should
give you a better idea what kind of information is stored in a SQL::SynaxModel:

	<root>
		<elements />
		<blueprints>
			<catalog id="1">
				<owner id="1" catalog="1" />
				<schema id="1" catalog="1" name="gene" owner="1">
					<domain id="1" schema="1" name="entity_id" base_type="NUM_INT" num_precision="9" />
					<domain id="2" schema="1" name="person_name" base_type="STR_CHAR" max_chars="100" char_enc="UTF8" />
					<table id="1" schema="1" name="person">
						<table_col id="1" table="1" name="person_id" domain="1" mandatory="1" default_val="1" auto_inc="1" />
						<table_col id="2" table="1" name="name" domain="2" mandatory="1" />
						<table_col id="3" table="1" name="father_id" domain="1" mandatory="0" />
						<table_col id="4" table="1" name="mother_id" domain="1" mandatory="0" />
						<table_ind id="1" table="1" name="primary" ind_type="UNIQUE">
							<table_ind_col id="1" table_ind="1" table_col="1" />
						</table_ind>
						<table_ind id="2" table="1" name="fk_father" ind_type="FOREIGN" f_table="1">
							<table_ind_col id="2" table_ind="2" table_col="3" f_table_col="1" />
						</table_ind>
						<table_ind id="3" table="1" name="fk_mother" ind_type="FOREIGN" f_table="1">
							<table_ind_col id="3" table_ind="3" table_col="4" f_table_col="1" />
						</table_ind>
					</table>
				</schema>
			</catalog>
			<application id="1" name="Setup">
				<catalog_link id="1" application="1" name="admin_link" target="1" />
				<command id="1" application="1" name="install_app_schema" command_type="DB_CREATE">
					<command_arg id="1" command="1" catalog_link="1" />
				</command>
				<command id="2" application="1" name="remove_app_schema" command_type="DB_DELETE">
					<command_arg id="2" command="2" catalog_link="1" />
				</command>
			</application>
			<application id="2" name="People Watcher">
				<catalog_link id="2" application="2" name="editor_link" target="1" />
				<routine id="1" routine_type="ANONYMOUS" application="2" name="fetch_all_persons" return_var_type="CURSOR">
					<view id="1" view_type="MATCH" name="fetch_all_persons" routine="1" match_all_cols="1">
						<view_src id="1" view="1" name="person" match_table="1" />
					</view>
					<routine_var id="1" routine="1" name="person_cursor" var_type="CURSOR" curs_view="1" />
					<routine_stmt id="1" routine="1" stmt_type="SPROC" call_sproc="CURSOR_OPEN">
						<routine_expr id="1" expr_type="VAR" p_stmt="1" routine_var="1" />
					</routine_stmt>
					<routine_stmt id="2" routine="1" stmt_type="RETURN">
						<routine_expr id="2" expr_type="VAR" p_stmt="2" routine_var="1" />
					</routine_stmt>
				</routine>
				<routine id="2" routine_type="ANONYMOUS" application="2" name="insert_a_person">
					<routine_arg id="1" routine="2" name="arg_person_id" var_type="SCALAR" domain="1" />
					<routine_arg id="2" routine="2" name="arg_person_name" var_type="SCALAR" domain="2" />
					<routine_arg id="3" routine="2" name="arg_father_id" var_type="SCALAR" domain="1" />
					<routine_arg id="4" routine="2" name="arg_mother_id" var_type="SCALAR" domain="1" />
					<view id="2" view_type="MATCH" name="insert_a_person" routine="2">
						<view_src id="2" view="2" name="person" match_table="1">
							<view_src_col id="1" src="2" match_table_col="1" />
							<view_src_col id="2" src="2" match_table_col="2" />
							<view_src_col id="3" src="2" match_table_col="3" />
							<view_src_col id="4" src="2" match_table_col="4" />
						</view_src>
						<view_expr id="1" expr_type="ARG" view="2" view_part="SET" set_view_col="1" routine_arg="1" />
						<view_expr id="2" expr_type="ARG" view="2" view_part="SET" set_view_col="2" routine_arg="2" />
						<view_expr id="3" expr_type="ARG" view="2" view_part="SET" set_view_col="3" routine_arg="3" />
						<view_expr id="4" expr_type="ARG" view="2" view_part="SET" set_view_col="4" routine_arg="4" />
					</view>
					<routine_stmt id="3" routine="2" stmt_type="SPROC" call_sproc="INSERT" view_for_dml="2" />
				</routine>
				<routine id="3" routine_type="ANONYMOUS" application="2" name="update_a_person">
					<routine_arg id="5" routine="3" name="arg_person_id" var_type="SCALAR" domain="1" />
					<routine_arg id="6" routine="3" name="arg_person_name" var_type="SCALAR" domain="2" />
					<routine_arg id="7" routine="3" name="arg_father_id" var_type="SCALAR" domain="1" />
					<routine_arg id="8" routine="3" name="arg_mother_id" var_type="SCALAR" domain="1" />
					<view id="3" view_type="MATCH" name="update_a_person" routine="3">
						<view_src id="3" view="3" name="person" match_table="1">
							<view_src_col id="5" src="3" match_table_col="1" />
							<view_src_col id="6" src="3" match_table_col="2" />
							<view_src_col id="7" src="3" match_table_col="3" />
							<view_src_col id="8" src="3" match_table_col="4" />
						</view_src>
						<view_expr id="5" expr_type="ARG" view="3" view_part="SET" set_view_col="6" routine_arg="6" />
						<view_expr id="6" expr_type="ARG" view="3" view_part="SET" set_view_col="7" routine_arg="7" />
						<view_expr id="7" expr_type="ARG" view="3" view_part="SET" set_view_col="8" routine_arg="8" />
						<view_expr id="8" expr_type="SFUNC" view="3" view_part="WHERE" call_sfunc="EQ">
							<view_expr id="9" expr_type="COL" p_expr="8" src_col="5" />
							<view_expr id="10" expr_type="ARG" p_expr="8" routine_arg="5" />
						</view_expr>
					</view>
					<routine_stmt id="4" routine="3" stmt_type="SPROC" call_sproc="UPDATE" view_for_dml="3" />
				</routine>
				<routine id="4" routine_type="ANONYMOUS" application="2" name="delete_a_person">
					<routine_arg id="9" routine="4" name="arg_person_id" var_type="SCALAR" domain="1" />
					<view id="4" view_type="MATCH" name="delete_a_person" routine="4">
						<view_src id="4" view="4" name="person" match_table="1">
							<view_src_col id="9" src="4" match_table_col="1" />
						</view_src>
						<view_expr id="11" expr_type="SFUNC" view="4" view_part="WHERE" call_sfunc="EQ">
							<view_expr id="12" expr_type="COL" p_expr="11" src_col="9" />
							<view_expr id="13" expr_type="ARG" p_expr="11" routine_arg="9" />
						</view_expr>
					</view>
					<routine_stmt id="5" routine="4" stmt_type="SPROC" call_sproc="DELETE" view_for_dml="4" />
				</routine>
			</application>
		</blueprints>
		<tools>
			<data_storage_product id="1" product_code="SQLite_2_8_12" is_file_based="1" />
			<data_storage_product id="2" product_code="Oracle_9_i" is_network_svc="1" />
			<data_link_product id="1" product_code="ODBC" />
		</tools>
		<sites>
			<catalog_instance id="1" product="1" blueprint="1" name="test">
				<user id="1" catalog="1" user_type="SCHEMA_OWNER" match_owner="1" name="ronsealy" password="K34dsD" />
				<user id="2" catalog="1" user_type="DATA_EDITOR" name="joesmith" password="fdsKJ4" />
			</catalog_instance>
			<application_instance id="1" blueprint="1" name="test Setup">
				<catalog_link_instance id="1" product="1" application="1" unrealized="1" target="1" local_dsn="test" />
			</application_instance>
			<application_instance id="2" blueprint="2" name="test People Watcher">
				<catalog_link_instance id="2" product="1" application="2" unrealized="2" target="1" local_dsn="test" />
			</application_instance>
			<catalog_instance id="2" product="2" blueprint="1" name="demo">
				<user id="3" catalog="2" user_type="SCHEMA_OWNER" match_owner="1" name="florence" password="0sfs8G" />
				<user id="4" catalog="2" user_type="DATA_EDITOR" name="thainuff" password="9340sd" />
			</catalog_instance>
			<application_instance id="3" blueprint="1" name="demo Setup">
				<catalog_link_instance id="3" product="1" application="3" unrealized="1" target="2" local_dsn="demo" />
			</application_instance>
			<application_instance id="4" blueprint="2" name="demo People Watcher">
				<catalog_link_instance id="4" product="1" application="4" unrealized="2" target="2" local_dsn="demo" />
			</application_instance>
		</sites>
		<circumventions />
	</root>

For some additional code samples, try looking at the various modules that
sub-class or use SQL::SyntaxModel.  They tend to implement or use wrappers that
make for much more compact code.

=head2 Comparative SQL Code Examples Generated From a Model

SQL::SyntaxModel works like an XML DOM except that it is restricted to holding
specific kinds of data, which resemble SQL statements.  This part of the
SYNOPSIS shows some actual SQL statements that can be generated from selected
portions of the above model.

This first set of Nodes describes 2 domains and 1 table, all 3 of which are
conceptually named schema objects.

	<domain id="1" schema="1" name="entity_id" base_type="NUM_INT" num_precision="9" />
	<domain id="2" schema="1" name="person_name" base_type="STR_CHAR" max_chars="100" char_enc="UTF8" />
	<table id="1" schema="1" name="person">
		<table_col id="1" table="1" name="person_id" domain="1" mandatory="1" default_val="1" auto_inc="1" />
		<table_col id="2" table="1" name="name" domain="2" mandatory="1" />
		<table_col id="3" table="1" name="father_id" domain="1" mandatory="0" />
		<table_col id="4" table="1" name="mother_id" domain="1" mandatory="0" />
		<table_ind id="1" table="1" name="primary" ind_type="UNIQUE">
			<table_ind_col id="1" table_ind="1" table_col="1" />
		</table_ind>
		<table_ind id="2" table="1" name="fk_father" ind_type="FOREIGN" f_table="1">
			<table_ind_col id="2" table_ind="2" table_col="3" f_table_col="1" />
		</table_ind>
		<table_ind id="3" table="1" name="fk_mother" ind_type="FOREIGN" f_table="1">
			<table_ind_col id="3" table_ind="3" table_col="4" f_table_col="1" />
		</table_ind>
	</table>

The above Node group has all the necessary details needed by external code to
generate the following SQL statements.  There are two versions of SQL given for
the same task; the first one is for SQL-2003 compliant databases, that support
DOMAIN schema objects; the second example is for older databases that do not. 
(Both of them use a MySQL extension AUTO_INCREMENT, but SQL generated for other
databases would do the same thing in a different way.)

	CREATE DOMAIN entity_id AS INTEGER(9);
	CREATE DOMAIN person_name AS VARCHAR(100);
	CREATE TABLE person (
		person_id entity_id NOT NULL DEFAULT 1 AUTO_INCREMENT,
		name person_name NOT NULL,
		father_id entity_id NULL,
		mother_id entity_id NULL,
		CONSTRAINT PRIMARY KEY (person_id),
		CONSTRAINT fk_father FOREIGN KEY (father_id) REFERENCES person (person_id),
		CONSTRAINT fk_mother FOREIGN KEY (mother_id) REFERENCES person (person_id)
	);

	CREATE TABLE person (
		person_id INTEGER(9) NOT NULL DEFAULT 1 AUTO_INCREMENT,
		name VARCHAR(100) NOT NULL,
		father_id INTEGER(9) NULL,
		mother_id INTEGER(9) NULL,
		CONSTRAINT PRIMARY KEY (person_id),
		CONSTRAINT fk_father FOREIGN KEY (father_id) REFERENCES person (person_id),
		CONSTRAINT fk_mother FOREIGN KEY (mother_id) REFERENCES person (person_id)
	);

Note that, regardless of which type of SQL is generated, the details for each
data type, including its name, only need to be declared once, in 'domain'
Nodes; if this one copy is changed, everything using it updates automatically.

This second set of Nodes describes a routine that takes 4 arguments (each of
which is an actual argument if a named stored procedure is generated, or a
named host parameter if un-named client-side SQL is generated) and performs an
UPDATE query against one table record; the query takes 4 arguments, using one
to match a record and 3 as new record column values to set.

	<routine id="3" routine_type="ANONYMOUS" application="2" name="update_a_person">
		<routine_arg id="5" routine="3" name="arg_person_id" var_type="SCALAR" domain="1" />
		<routine_arg id="6" routine="3" name="arg_person_name" var_type="SCALAR" domain="2" />
		<routine_arg id="7" routine="3" name="arg_father_id" var_type="SCALAR" domain="1" />
		<routine_arg id="8" routine="3" name="arg_mother_id" var_type="SCALAR" domain="1" />
		<view id="3" view_type="MATCH" name="update_a_person" routine="3">
			<view_src id="3" view="3" name="person" match_table="1">
				<view_src_col id="5" src="3" match_table_col="1" />
				<view_src_col id="6" src="3" match_table_col="2" />
				<view_src_col id="7" src="3" match_table_col="3" />
				<view_src_col id="8" src="3" match_table_col="4" />
			</view_src>
			<view_expr id="5" expr_type="ARG" view="3" view_part="SET" set_view_col="6" routine_arg="6" />
			<view_expr id="6" expr_type="ARG" view="3" view_part="SET" set_view_col="7" routine_arg="7" />
			<view_expr id="7" expr_type="ARG" view="3" view_part="SET" set_view_col="8" routine_arg="8" />
			<view_expr id="8" expr_type="SFUNC" view="3" view_part="WHERE" call_sfunc="EQ">
				<view_expr id="9" expr_type="COL" p_expr="8" src_col="5" />
				<view_expr id="10" expr_type="ARG" p_expr="8" routine_arg="5" />
			</view_expr>
		</view>
		<routine_stmt id="4" routine="3" stmt_type="SPROC" call_sproc="UPDATE" view_for_dml="3" />
	</routine>

The above Node group, *together* with the previous Node group, has details to
generate the following SQL statements.  There are two versions of SQL given for
the same task; the first one is for databases that support named bind
variables, illustrated using the Oracle style of ':foo'; the second one is for
those that require positional host parameters, illustrated with the DBI style of
'?'.  These two SQL variants are intended to be run by the SQL client.

	UPDATE person
	SET name = :arg_person_name, father_id = :arg_father_id, mother_id = :arg_mother_id
	WHERE person_id = :arg_person_id;

	UPDATE person
	SET name = ?, father_id = ?, mother_id = ?
	WHERE person_id = ?;

Alternately, a stored procedure (and calls to it) can be generated from the
same SSM Node set, if the routine_type attribute is PROCEDURE instead of
ANONYMOUS.  The two SQL variants are for new or old databases respectively,
like the first example.

	CREATE PROCEDURE update_a_person
	(arg_person_id entity_id, arg_person_name person_name, arg_father_id entity_id, arg_mother_id entity_id)
	BEGIN
		UPDATE person
		SET name = arg_person_name, father_id = arg_father_id, mother_id = arg_mother_id
		WHERE person_id = arg_person_id;
	END;

	CREATE PROCEDURE update_a_person
	(arg_person_id INTEGER(9), arg_person_name VARCHAR(100), arg_father_id INTEGER(9), arg_mother_id INTEGER(9))
	BEGIN
		UPDATE person
		SET name = arg_person_name, father_id = arg_father_id, mother_id = arg_mother_id
		WHERE person_id = arg_person_id;
	END;

To go with those, here are SQL statements to invoke the server-side stored
procedures, with the two variants being named-vs-positional host parameters.

	CALL update_a_person (:arg_person_id, :arg_person_name, :arg_father_id, :arg_mother_id);

	CALL update_a_person (?, ?, ?, ?);

Finally, all DROP statements can be generated from the same Nodes as CREATE.

Note that one key feature of SQL::SyntaxModel is that all of a model's pieces
are linked by references rather than by name as in SQL itself.  So if you
wanted to change the name of a table column, such as 'person_name' to
'the_name', then you make the change in exactly one place and all SQL generated
from the model will update, both the CREATE and UPDATE statements. Alternately,
if you wanted to change the data type of person ids, then you only have to make
a single change, such as by setting num_precision to 6.  Alternately, if you
wanted to change the order of the arguments for 'update_a_person', you only
have to change the order the 'routine_arg' Nodes appear, and any calls to the
procedure will automatically re-order any passed values in the generated SQL.

I<See also the separately distributed Rosetta::Utility::SQLBuilder module,
which is a reference implementation of a SQL generator for SQL::SyntaxModel.>

=head1 DESCRIPTION

The SQL::SyntaxModel Perl 5 module is intended to be a powerful but easy to use
replacement for SQL strings (including support for placeholders), which you can
use to make queries against a database.  Each SQL::SyntaxModel object can
represent a non-ambiguous rigorously structured command for a database to
execute, or one can be a non-ambiguous rigorously structured description of a
database schema object. This class supports all types of database operations,
including both data manipulation and schema manipulation, as well as managing
database instances and users.  You typically construct a database query by
setting appropriate attributes of these objects, and you execute a database
query by evaluating the same attributes.  SQL::SyntaxModel objects are designed
to be equivalent to SQL in both the type of information they carry and in their
conceptual structure. This is analagous to how XML DOMs are objects that are
equivalent to XML strings, and they can be converted back and forth at will. 
If you know SQL, or even just relational database theory in general, then this
module should be easy to learn.

SQL::SyntaxModels are intended to represent all kinds of SQL, both DML and DDL,
both ANSI standard and RDBMS vendor extensions.  Unlike basically all of the
other SQL generating/parsing modules I know about, which are limited to basic
DML and only support table definition DDL, this class supports arbitrarily
complex select statements, with composite keys and unions, and calls to stored
functions; this class can also define views and stored procedures and triggers.
Some of the existing modules, even though they construct complete SQL, will
take/require fragments of SQL as input (such as "where" clauses)  By contrast,
SQL::SyntaxModel takes no SQL fragments.  All of its inputs are atomic, which
means it is also easier to analyse the objects for implementing a wider range
of functionality than previously expected; for example, it is much easier to
analyse any select statement and generate update/insert/delete statements for
the virtual rows fetched with it (a process known as updateable views).

Considering that each database product has its own dialect of SQL which it
implements, you would have to code SQL differently depending on which database
you are using.  One common difference is the syntax for specifying an outer
join in a select query.  Another common difference is how to specify that a
table column is an integer or a boolean or a character string.  Moreover, each
database has a distinct feature set, so you may be able to do tasks with one
database that you can't do with another.  In fact, some databases don't support
SQL at all, but have similar features that are accessible thorough alternate
interfaces. SQL::SyntaxModel is designed to represent a normalized superset of
all database features that one may reasonably want to use.  "Superset" means
that if even one database supports a feature, you will be able to invoke it
with this class. You can also reference some features which no database
currently implements, but it would be reasonable for one to do so later.
"Normalized" means that if multiple databases support the same feature but have
different syntax for referencing it, there will be exactly one way of referring
to it with SQL::SyntaxModel.  So by using this class, you will never have to
change your database-using code when moving between databases, as long as both
of them support the features you are using (or they are emulated).  That said,
it is generally expected that if a database is missing a specific feature that
is easy to emulate, then code which evaluates SQL::SyntaxModels will emulate it
(for example, emulating "left()" with "substr()"); in such cases, it is
expected that when you use such features they will work with any database.  For
example, if you want a model-specified BOOLEAN data type, you will always get
it, whether it is implemented  on a per-database-basis as a "boolean" or an
"int(1)" or a "number(1,0)".  Or a model-specified "STR_CHAR" data type you will
always get it, whether it is called "text" or "varchar2" or "sql_varchar".

SQL::SyntaxModel is intended to be just a stateless container for database
query or schema information.  It does not talk to any databases by itself and
it does not generate or parse any SQL; rather, it is intended that other third
party modules or code of your choice will handle this task.  In fact,
SQL::SyntaxModel is designed so that many existing database related modules
could be updated to use it internally for storing state information, including
SQL generating or translating modules, and schema management modules, and
modules which implement object persistence in a database.  Conceptually
speaking, the DBI module itself could be updated to take SQL::SyntaxModel
objects as arguments to its "prepare" method, as an alternative (optional) to
the SQL strings it currently takes.  Code which implements the things that
SQL::SyntaxModel describes can do this in any way that they want, which can
mean either generating and executing SQL, or generating Perl code that does the
same task and evaling it, should they want to (the latter can be a means of
emulation).  This class should make all of that easy.

SQL::SyntaxModel is especially suited for use with applications or modules that
make use of data dictionaries to control what they do.  It is common in
applications that they interpret their data dictionaries and generate SQL to
accomplish some of their work, which means making sure generated SQL is in the
right dialect or syntax, and making sure literal values are escaped correctly.
By using this module, applications can simply copy appropriate individual
elements in their data dictionaries to SQL::SyntaxModel properties, including
column names, table names, function names, literal values, host parameter names,
and they don't have to do any string parsing or assembling.

Now, I can only imagine why all of the other SQL generating/parsing modules
that I know about have excluded privileged support for more advanced database
features like stored procedures.  Either the authors didn't have a need for it,
or they figured that any other prospective users wouldn't need it, or they
found it too difficult to implement so far and maybe planned to do it later. As
for me, I can see tremendous value in various advanced features, and so I have
included privileged support for them in SQL::SyntaxModel.  You simply have to
work on projects of a significant size to get an idea that these features would
provide a large speed, reliability, and security savings for you.  Look at many
large corporate or government systems, such as those which have hundreds of
tables or millions of records, and that may have complicated business logic
which governs whether data is consistent/valid or not.  Within reasonable
limits, the more work you can get the database to do internally, the better.  I
believe that if these features can also be represented in a database-neutral
format, such as what SQL::SyntaxModel attempts to do, then users can get the
full power of a database without being locked into a single vendor due to all
their investment in vendor-specific SQL stored procedure code.  If customers
can move a lot more easily, it will help encourage database vendors to keep
improving their products or lower prices to keep their customers, and users in
general would benefit.  So I do have reasons for trying to tackle the advanced
database features in SQL::SyntaxModel.

=head1 CLASSES IN THIS MODULE

This module is implemented by several object-oriented Perl 5 packages, each of
which is referred to as a class.  They are: B<SQL::SyntaxModel> (the module's
name-sake), B<SQL::SyntaxModel::Container> (aka B<Container>, aka B<Model>),
and B<SQL::SyntaxModel::Node> (aka B<Node>).

I<While all 3 of the above classes are implemented in one module for
convenience, you should consider all 3 names as being "in use"; do not create
any modules or packages yourself that have the same names.>

The Container and Node classes do most of the work and are what you mainly use.
 The name-sake class mainly exists to guide CPAN in indexing the whole module,
but it also provides a set of stateless utility methods and constants that the
other two classes inherit, and it provides a few wrapper functions over the
other classes for your convenience; you never instantiate an object of
SQL::SyntaxModel itself.

=head1 STRUCTURE

The internal structure of a SQL::SyntaxModel object is conceptually a cross
between an XML DOM and an object-relational database, with a specific schema.
This module is implemented with two main classes that work together, Containers
and Nodes. The Container object is an environment or context in which Node
objects usually live.  A typical application will only need to create one
Container object (returned by the module's 'new' function), and then a set of
Nodes which live within that Container.  The Nodes are related sometimes with
single or multiple cardinality to each other.

SQL::SyntaxModel is expressly designed so that its data is easy to convert
between different representations, mainly in-memory data structures linked by
references, and multi-table record sets stored in relational databases, and
node sets in XML documents.  A Container corresponds to an XML document or a
complete database, and each Node corresponds to an XML node or a database
record.  Each Node has a specific node_type (a case-sensitive string), which
corresponds to a database table or an XML tag name.  See the
SQL::SyntaxModel::Language documentation file to see which ones exist.  The
node_type is set when the Node is created and it can not be changed later.

A Node has a specific set of allowed attributes that are determined by the
node_type, each of which corresponds to a database table column or an XML node
attribute.  Every Node of a common node_type has a unique 'id' attribute (a
positive integer) by which it is referenced; that attribute corresponds to the
database table's single-column primary key.  Each other Node attribute is
either a scalar value of some data type, or an enumerated value, or a reference
to another Node of a specific node_type, which has a foreign-key constraint on
it.  Foreign-key constraints are enforced by this module, so you will have to
add Nodes in the appropriate order, just as when adding records to a database.
Any Node which is referenced in an attribute (cited in a foreign-key
constraint) of another is a parent of the other; as a corollary, the second
Node is a child of the first.  The order of child Nodes under a parent is the
same as that in which the parent-child relationship was assigned, unless you 
have afterwards used the move_before_sibling() method to change this.

I<In versions of SQL::SyntaxModel prior to the 2004-02-04 release, some Node
types also had explicit 'order' attributes so that if each Node was converted
as-is to an RDBMS record, it would be possible to retrieve the records in the
same sequence; this was useful in cases where the order of Nodes was important.
These redundant 'order' attributes were eliminated as of 2004-02-04 since their
maintenance was making this module more difficult to use.  As a consequence,
you will now have to add a column yourself to maintain the sort order when
converting Nodes to RDBMS records.>

When SQL::SyntaxModels are converted to XML, one referencing attribute is given
higher precedence than the others and becomes the single parent XML node.  For
example, the XML parent of a 'table_col' Node is always a 'table' Node, even
though a 'domain' Node is also referenced.  While Nodes of most types always
have Nodes of a single other type as their parents, there are some exceptions.
Nodes of certain types, such as view or *_expr, may have either another
Node of the same type as itself, or of a specific other type as its parent,
depending on the context; these Nodes form trees of their own type, and it is
the root Node of each tree which has a different Node type as its parent. 

Finally, any Node of certain types will always have a specific pseudo-node as
its single parent, which it does not reference in an attribute, and which can
not be changed.  All 6 pseudo-nodes have no attributes, even 'id', and only one
of each exists; they are created by default with the Container they are part
of, forming the top 2 levels of the Node tree, and can not be removed.  They
are: 'root' (the single level-1 Node which is parent to the other pseudo-nodes
but no normal Nodes), 'elements' (parent to 'domain' Nodes), 'blueprints'
(parent to 'catalog' and 'application' Nodes), 'tools' (parent to
'data_storage_product' and 'data_link_product' Nodes), 'sites' (parent to
'catalog_instance' and 'application_instance' Nodes), and 'circumventions'
(parent to 'sql_fragment' nodes).  All other Node types have normal Nodes as
parents.

You should look at the POD-only file named SQL::SyntaxModel::Language, which
comes with this distribution.  It serves to document all of the possible Node
types, with attributes, constraints, and allowed relationships with other Node
types.  As the SQL::SyntaxModel class itself has very few properties and
methods, all being highly generic (much akin to an XML DOM), the POD of this PM
file will only describe how to use said methods, and will not list all the
allowed inputs or constraints to said methods.  With only simple guidance in
SyntaxModel.pm, you should be able to interpret Language.pod to get all the
nitty gritty details.  You should also look at the tutorial or example files
which will be in the distribution when ready.  You could also learn something
from the code samples inside other modules which sub-class this one.

=head1 FAULT TOLERANCE AND MULTI-THREADING SUPPORT

I<Disclaimer: The following claims assume that only this module's published API
is used, and that you do not set object properties directly or call private
methods, which Perl does not prevent.  It also assumes that the module is bug
free, and that any errors or warnings which appear while the code is running
are thrown explicitly by this module as part of its normal functioning.>

SQL::SyntaxModel is designed to ensure that the objects it produces are always
internally consistant, and that the data they contain is always well-formed,
regardless of the circumstances in which it is used.  You should be able to 
fetch data from the objects at any time and that data will be self-consistant 
and well-formed.  

This will not change regardless of what kind of bad input data you provide to
object methods or module functions.  Providing bad input data will cause the
module to throw an exception; if you catch this and the program continues
running (such as to chide the user and have them try entering correct input),
then the objects will remain un-corrupted and able to accept new input or give
proper output.  In most cases, the object will be in the same state as it was 
before the public method was called with the bad input.

This module does not use package variables at all, besides constants like
$VERSION, and all symbols ($@%) declared at file level are strictly constant
value declarations.  No object should ever step on another.

This module will allow a Node to be created piecemeal, such as when it is
storing details gathered one at a time from the user, and during this time some
mandatory Node properties may not be set, or pending links from this node to
others may not be validated.  However, until a Node has its required properties
set and/or its Node links are validated, no references will be made to this
Node from other Nodes; from their point of view it doesn't exist, and hence the
other Nodes are all consistant.

SQL::SyntaxModel is explicitly not thread-aware (thread-safe); it contains no
code to synchronize access to its objects' properties, such as semaphores or
locks or mutexes.  To internalize such things in an effective manner would have
made the code a lot more complex than it is now, without any clear benefits.  
However, this module can (and should) be used in multi-threaded environments 
where the application/caller code takes care of synchronizing access to its 
objects, especially if the application uses coarse-grained read or write locks.

The author's expectation is that this module will be mainly used in
circumstances where the majority of actions are reads, and there are very few
writes, such as with a data dictionary; perhaps all the writes on an object may
be when it is first created.  An application thread would obtain a read
lock/semaphore on a Container object during the period for which it needs to
ensure read consistency; it would block write lock attempts but not other read
locks.  It would obtain a write lock during the (usually short) period it needs
to change something, which blocks all other lock attempts (for read or write).

An example of this is a web server environment where each page request is being
handled by a distinct thread, and all the threads share one SQL::SyntaxModel
object; normally the object is instantiated when the server starts, and the
worker threads then read from it for guidance in using a common database.
Occasionally a thread will want to change the object, such as to correspond to
a simultaneous change to the database schema, or to the web application's data
dictionary that maps the database to application screens.  Under this
situation, the application's definitive data dictionary (stored partly or
wholly in a SQL::SyntaxModel) can occupy one place in RAM visible to all
threads, and each thread won't have to keep looking somewhere else such as in
the database or a file to keep up with the definitive copy.  (Of course, any
*changes* to the in-memory data dictionary should see a corresponding update to
a non-volatile copy, like in an on-disk database or file.)

I<Note that, while a nice thing to do may be to manage a course-grained lock in
SQL::SyntaxModel, with the caller invoking lock_to_read() or lock_to_write() or
unlock() methods on it, Perl's thread-E<gt>lock() mechanism is purely context
based; the moment lock_to_...() returns, the object has unlocked again.  Of
course, if you know a clean way around this, I would be happy to hear it.>

=head1 NODE EVOLUTION STATES

A SQL::SyntaxModel Node object always exists in one of 3 official ordered
states (which can conceptually be divided further into more states).  For now
we can call them "Alone" (1), "At Home" (2), and "Well Known" (3).  (Hey, that
rhymes!)  The set of legal operations you can perform on a Node are different
depending on its state, and a Node can only transition between
adjacent-numbered states one at a time.

When a new Node is created, using new_node(), it starts out "Alone"; it does
*not* live in a Container, and it is illegal to have any actual (Perl)
references between it and any other Node.  Nodes in this state can be built
(have their Node Id and other attributes set or changed) piecemeal with the
least processing overhead, and can be moved or exist independently of anything
else that SQL::SyntaxModel manages.  An "Alone" Node does not need to have its
Node Id set.  Any Node attributes which are conceptually references to other
Nodes are stored and read as Id numbers when the Node is "Alone"; also, no
confirmation has yet taken place that the referenced Nodes actually exist yet.
A Node may only be individually deleted when it is "Alone"; in this state it
will be garbage collected like any Perl variable when your own reference to it
goes away.

When you invoke the put_in_container() method on an "Alone" Node, giving it a
Container object as an argument, the Node will transition to the "At Home"
state; you can move from "At Home" to "Alone" using the complementary
take_from_container() method.  An "At Home" Node lives in a Container, and any
attributes which refer to other Nodes now must be actual references, where the
existence of the other Node in the same Container is confirmed.  If any
conceptual references are set in a Node while it is "Alone", these will be
converted into actual references by put_in_container(), which will fail if any
can't be found.  take_from_container() will replace references with Node Ids. A
Node can only link to a Node in the same Container as itself.  While a Node in
"At Home" status can link to other Nodes, those Nodes can not link back to an
"At Home" Node in their own child list; from their point of view, the "At Home"
Node doesn't exist.  In addition, an "At Home" Node can not have children of 
its own; it can not be referenced by any other Nodes.

When you invoke the add_reciprocal_links() method on an "At Home" Node, the
Node will transition to the "Well Known" state; any other Nodes that this one
references will now link back to it in their own child lists.  The
complementary remove_reciprocal_links() method will break those return links
and transition a "Well Known" Node to an "At Home" one.  A "Well Known" Node 
is also allowed to have children of its own.

Testing for the existence of mandatory Node attribute values is separate from 
the official Node state and can be invoked on a Node at any time.  None of the 
official Node states themselves will assert that any mandatory attributes are 
populated.  This testing is separate partly to make it easy for you to build 
Nodes piecemeal, though there are other practical reasons for it.

Note that all typical Node attributes can be read, set, replaced, or cleared at
any time regardless of the Node state; you can set them all either when the
Node is "Alone" or when it is "Well Known", as is your choice.  However, the
Node Id must always have a value when the Node is in a Container; if you want
to make a Node "Well Known" as early as possible, you simply have to set its
Node Id first.

=head1 SYNTAX

This class does not export any functions or methods, so you need to call them
using object notation.  This means using B<Class-E<gt>function()> for functions
and B<$object-E<gt>method()> for methods.  If you are inheriting this class for
your own modules, then that often means something like B<$self-E<gt>method()>.  

All SQL::SyntaxModel functions and methods are either "getters" (which read and
return or generate values but do not change the state of anything) or "setters"
(which change the state of something but do not return anything on success);
none do getting or setting conditionally based on their arguments.  While this
means there are more methods in total, I see this arrangement as being more
stable and reliable, plus each method is simpler and easier to understand or
use; argument lists and possible return values are also less variable and more
predictable.

All "setter" functions or methods which are supposed to change the state of
something will throw an exception on failure (usually from being given bad
arguments); on success, they officially have no return values.  A thrown
exception will always include details of what went wrong (and where and how) in
a machine-readable (and generally human readable) format, so that calling code
which catches them can recover gracefully.  The methods are all structured so
that they check all preconditions prior to changing any state information, and
so one can assume that upon throwing an exception, the Node and Container
objects are in a consistent or recoverable state at worst, and are completely
unchanged at best.

All "getter" functions or methods will officially return the value or construct
that was asked for; if said value doesn't (yet or ever) exist, then this means
the Perl "undefined" value.  When given bad arguments, generally this module's
"information" functions will return the undefined value, and all the other
functions/methods will throw an exception like the "setter" functions do.

Generally speaking, if SQL::SyntaxModel throws an exception, it means one of
two things: 1. Your own code is not invoking it correctly, meaning you have
something to fix; 2. You have decided to let it validate some of your input
data for you (which is quite appropriate).  

Note also that SQL::SyntaxModel is quite strict in its own argument checking,
both for internal simplicity and robustness, and so that code which *reads* 
data from it can be simpler.  If you want your own program to be more liberal
in what input it accepts, then you will have to bear the burden of cleaning up
or interpreting that input, or delegating such work elsewhere.  (Or perhaps 
someone may want to make a wrapper module to do this?)

=head1 CONSTRUCTOR WRAPPER FUNCTIONS

These functions are stateless and can be invoked off of either the module name,
or any package name in this module, or any object created by this module; they
are thin wrappers over other methods and exist strictly for convenience.

=head2 new_container()

	my $model = SQL::SyntaxModel->new_container();
	my $model2 = SQL::SyntaxModel::Container->new_container();
	my $model3 = SQL::SyntaxModel::Node->new_container();
	my $model4 = $model->new_container();
	my $model5 = $node->new_container();

This function wraps SQL::SyntaxModel::Container->new().

=head2 new_node( NODE_TYPE )

	my $node = SQL::SyntaxModel->new_node( 'table' );
	my $node2 = SQL::SyntaxModel::Container->new_node( 'table' );
	my $node3 = SQL::SyntaxModel::Node->new_node( 'table' );
	my $node4 = $model->new_node( 'table' );
	my $node5 = $node->new_node( 'table' );

This function wraps SQL::SyntaxModel::Node->new( NODE_TYPE ).

=head1 CONTAINER CONSTRUCTOR FUNCTIONS AND METHODS

This function/method is stateless and can be invoked off of either the Container
class name or an existing Container object, with the same result.

=head2 new()

	my $model = SQL::SyntaxModel::Container->new();
	my $model2 = $model->new();

This "getter" function/method will create and return a single
SQL::SyntaxModel::Container (or subclass) object.

=head1 CONTAINER OBJECT METHODS

These methods are stateful and may only be invoked off of Container objects.

=head2 destroy()

	$model->destroy();

This "setter" method will destroy the Container object that it is invoked from,
and it will also destroy all of the Nodes inside that Container.  This method
exists because all Container objects (having 1 or more Node) contain circular
references between the Container and all of its Nodes.  You need to invoke this
method when you are done with a Container, or you will leak the memory it uses
when your external references to it go out of scope.  This method can be
invoked at any time and will not throw any exceptions.  When it has completed,
all external references to the Container or any of its Nodes will each point to
an empty (but still blessed) Perl hash.  I<See the CAVEATS documentation.>

=head2 get_node( NODE_TYPE, NODE_ID )

	my $catalog_node = $model->get_node( 'catalog', 1 );

This "getter" method returns a reference to one of this Container's member
Nodes, which has a Node Type of NODE_TYPE, and a Node Id of NODE_ID.  You may
not request a pseudo-node (it doesn't actually exist).

=head2 get_child_nodes([ NODE_TYPE ])

	my $ra_node_list = $model->get_child_nodes();
	my $ra_node_list = $model->get_child_nodes( 'catalog' );

This "getter" method returns a list of this Container's child Nodes, in a new
array ref.  A Container's child Nodes are defined as being all Nodes in the
Container whose Node Type defines them as always having a pseudo-Node parent. 
If the optional argument NODE_TYPE is defined, then only child Nodes of that
Node Type are returned; otherwise, all child Nodes are returned.  All Nodes are
returned in the same order they were added.

=head2 get_next_free_node_id( NODE_TYPE )

	my $node_id = $model->get_next_free_node_id( 'catalog' );

This "getter" method returns an integer which is valid for use as the Node ID
of a new Node, which has a Node Type of NODE_TYPE, that is going to be put in
this Container.  Its value is 1 higher than the highest Node ID for the same
Node Type that is already in the Container, or had been before.  You can use
this method like a sequence generator to produce Node Ids for you rather than
you producing them in some other way.  An example situation when this method
would be useful is if you are building a SQL::SyntaxModel by scanning the
schema of an existing database.

=head2 deferrable_constraints_are_tested()

	my $is_all_ok = $model->deferrable_constraints_are_tested();

This "getter" method will return the boolean "deferrable constraints are
tested" property of this Container.  This property is true when all "Well
Known" Nodes in this Container are known to be free of all data errors, both
individually and collectively.  This property is initially set to true when a
Container is new and empty; it is also set to true by
Container.test_deferrable_constraints() when all of its tests complete without
finding any problems.  This property is set to false when any changes are made
to a "Well Known" Node in this Container, which includes moving the Node in to
or out of "Well Known" status.

=head2 test_deferrable_constraints()

	$model->test_deferrable_constraints();

This "getter" method implements several types of deferrable data validation, to
make sure that every "Well Known" Node in this Container is ready to be used,
both individually and collectively; it throws an exception if it can find
anything wrong.  Note that a failure with any one Node will cause the testing
of the whole set to abort, as the offending Node throws an exception which this
method doesn't catch; any untested Nodes could also have failed, so you will
have to re-run this method after fixing the problem.  This method ignores any
"At Home" Nodes in this Container, and runs its collective tests as if they
didn't exist.  This method will short-circuit and not perform any tests if this
Container's "deferrable constraints are tested" property is true, so to avoid
unnecessary repeated tests due to redundant external invocations; this allows
you to put validation checks for safety everywhere in your program while
avoiding a corresponding performance hit.

=head1 NODE CONSTRUCTOR FUNCTIONS AND METHODS

This function/method is stateless and can be invoked off of either the Node
class name or an existing Node object, with the same result.

=head2 new( NODE_TYPE )

	my $node = SQL::SyntaxModel::Node->new( 'table' );
	my $node2 = $node->new( 'table' );

This "getter" function/method will create and return a single
SQL::SyntaxModel::Node (or subclass) object whose Node Type is given in the
NODE_TYPE (enum) argument, and all of whose other properties are defaulted to
an "empty" state.  A Node's type can only be set on instantiation and can not
be changed afterwards; only specific values are allowed, which you can see in
the SQL::SyntaxModel::Language documentation file.  This new Node does not yet
live in a Container, and will have to be put in one later before you can make
full use of it.  However, you can read or set or clear any or all of this new
Node's attributes (including the Node Id) prior to putting it in a Container,
making it easy to build one piecemeal before it is actually "used".  A Node can
not have any actual Perl references between it and other Nodes until it is in a
Container, and as such you can delete it simply by letting your own reference
to it be garbage collected.

=head1 NODE OBJECT METHODS

These methods are stateful and may only be invoked off of Node objects.  For
some of these, it doesn't matter whether the Node is in a Container or not, nor
whether its links to other Nodes are reciprocated or not.  For others, one or
both of these conditions must be true or false for the method to be invoked, or
it will throw an exception (like for bad input).

=head2 delete_node()

This "setter" method will destroy the Node object that it is invoked from, if
it can.  You are only allowed to delete Nodes that are not inside Containers,
and which don't have child Nodes; failing this, you must remove the children
and then take this Node from its Container first.  Technically, this method
doesn't actually do anything (pure-Perl version) other than validate that you
are allowed to delete; when said conditions are met, the Node will be garbage
collected as soon as you lose your reference to it.

=head2 get_node_type()

	my $type = $node->get_node_type();

This "getter" method returns the Node Type scalar (enum) property of this Node.
 You can not change this property on an existing Node, but you can set it on a
new one.

=head2 get_node_id()

This "getter" method will return the integral Node Id property of this Node, 
if it has one.

=head2 clear_node_id()

This "setter" method will erase this Node's Id property if it can.  A Node's Id
may only be cleared if the Node is not in a Container.

=head2 set_node_id( NEW_ID )

This "setter" method will set or replace this Node's Id property if it can.  If 
this Node is in a Container, then the replacement will fail if some other Node 
with the same Node Type and Node Id already exists in the same Container.

=head2 expected_literal_attribute_type( ATTR_NAME )

This "getter" method will return an enumerated value that explains which
literal data type that values for this Node's literal attribute named in the
ATTR_NAME argument must be.

=head2 get_literal_attribute( ATTR_NAME )

This "getter" method will return the value for this Node's literal attribute named
in the ATTR_NAME argument.

=head2 get_literal_attributes()

This "getter" method will fetch all of this Node's literal attributes, 
returning them in a Hash ref.

=head2 clear_literal_attribute( ATTR_NAME )

This "setter" method will clear this Node's literal attribute named in
the ATTR_NAME argument.

=head2 clear_literal_attributes()

This "setter" method will clear all of this Node's literal attributes.

=head2 set_literal_attribute( ATTR_NAME, ATTR_VALUE )

This "setter" method will set or replace this Node's literal attribute named in
the ATTR_NAME argument, giving it the new value specified in ATTR_VALUE.

=head2 set_literal_attributes( ATTRS )

This "setter" method will set or replace multiple Node literal attributes,
whose names and values are specified by keys and values of the ATTRS hash ref
argument; this method will invoke set_literal_attribute() for each key/value
pair.

=head2 expected_enumerated_attribute_type( ATTR_NAME )

This "getter" method will return an enumerated value that explains which
enumerated data type that values for this Node's enumerated attribute named in the
ATTR_NAME argument must be.

=head2 get_enumerated_attribute( ATTR_NAME )

This "getter" method will return the value for this Node's enumerated attribute
named in the ATTR_NAME argument.

=head2 get_enumerated_attributes()

This "getter" method will fetch all of this Node's enumerated attributes,
returning them in a Hash ref.

=head2 clear_enumerated_attribute( ATTR_NAME )

This "setter" method will clear this Node's enumerated attribute named in the
ATTR_NAME argument.

=head2 clear_enumerated_attributes()

This "setter" method will clear all of this Node's enumerated attributes.

=head2 set_enumerated_attribute( ATTR_NAME, ATTR_VALUE )

This "setter" method will set or replace this Node's enumerated attribute named in
the ATTR_NAME argument, giving it the new value specified in ATTR_VALUE.

=head2 set_enumerated_attributes( ATTRS )

This "setter" method will set or replace multiple Node enumerated attributes,
whose names and values are specified by keys and values of the ATTRS hash ref
argument; this method will invoke set_enumerated_attribute() for each key/value
pair.

=head2 expected_node_ref_attribute_type( ATTR_NAME )

This "getter" method will return an enumerated value that explains which Node
Type that values for this Node's node attribute named in the ATTR_NAME argument
must be.

=head2 get_node_ref_attribute( ATTR_NAME )

This "getter" method will return the value for this Node's node attribute
named in the ATTR_NAME argument.  The value will be a Node ref if the current 
Node is in a Container, and an Id number if it isn't.

=head2 get_node_ref_attributes()

This "getter" method will fetch all of this Node's node attributes,
returning them in a Hash ref.  The values will be Node refs if the current 
Node is in a Container, and Id numbers if it isn't.

=head2 clear_node_ref_attribute( ATTR_NAME )

This "setter" method will clear this Node's node attribute named in the
ATTR_NAME argument.  If the other Node being referred to has a reciprocal 
link to the current one in its child list, that will also be cleared.

=head2 clear_node_ref_attributes()

This "setter" method will clear all of this Node's node attributes; see 
the clear_node_ref_attribute() documentation for the semantics.

=head2 set_node_ref_attribute( ATTR_NAME, ATTR_VALUE )

This "setter" method will set or replace this Node's node attribute named in
the ATTR_NAME argument, giving it the new value specified in ATTR_VALUE (if it
is different).  If the attribute was previously valued, this method will first
invoke clear_node_ref_attribute() on it.  When setting a new value, if the current
Node is in a Container and expects Nodes it links to reciprocate, then it will
also add the current Node to the other Node's child list.

=head2 set_node_ref_attributes( ATTRS )

This "setter" method will set or replace multiple Node node attributes,
whose names and values are specified by keys and values of the ATTRS hash ref
argument; this method will invoke set_node_ref_attribute() for each key/value
pair.

=head2 expected_attribute_major_type( ATTR_NAME )

This "getter" method will return an enumerated value that explains which major
data type that values for this Node's attribute named in the ATTR_NAME argument
must be.  There are 4 possible return values: 'ID' (the Node Id), 'LITERAL' (a
literal attribute), 'ENUM' (an enumerated attribute), and 'NODE' (a node ref 
attribute).

=head2 get_attribute( ATTR_NAME )

	my $curr_val = $node->get_attribute( 'name' );

This "getter" method will return the value for this Node's attribute named in
the ATTR_NAME argument.

=head2 get_attributes()

	my $rh_attrs = $node->get_attributes();

This "getter" method will fetch all of this Node's attributes, returning them
in a Hash ref.

=head2 clear_attribute( ATTR_NAME )

This "setter" method will clear this Node's attribute named in the ATTR_NAME
argument.

=head2 clear_attributes()

This "setter" method will clear all of this Node's attributes.

=head2 set_attribute( ATTR_NAME, ATTR_VALUE )

This "setter" method will set or replace this Node's attribute named in the
ATTR_NAME argument, giving it the new value specified in ATTR_VALUE.

=head2 set_attributes( ATTRS )

	$node->set_attributes( $rh_attrs );

This "setter" method will set or replace multiple Node attributes, whose names
and values are specified by keys and values of the ATTRS hash ref argument;
this method will invoke set_attribute() for each key/value pair.

=head2 get_parent_node_attribute_name()

This "getter" method returns the name of this Node's node attribute which is
designated to reference this Node's primary parent Node, if there is one.

=head2 get_parent_node()

	my $parent = $node->get_parent_node();

This "getter" method returns the primary parent Node of the current Node, if
there is one.  The semantics are like "if the current Node is in a Container
and its 'parent node attribute name' is defined, then return the Node ref value
of the named node attribute, if it has one".

=head2 clear_parent_node_attribute_name()

This "setter" method will clear this Node's 'primary pparent node attribute
name' property, if it has one.  The actual node attribute being referred to is
not affected.

=head2 set_parent_node_attribute_name( ATTR_NAME )

This "setter" method will set or replace this Node's 'primary parent node attribute
name' property, giving it the new value specified in ATTR_NAME.  No actual node
attribute is affected.  Note that only a subset (usually one) of a Node's node
attributes may be named as the holder of its primary parent.

=head2 estimate_parent_node_attribute_name( NEW_PARENT[, ONLY_NOT_VALUED] )

This "getter" method will try to find a way to make the Node given in its
NEW_PARENT argument into the primary parent of the current Node.  It returns
the name of the first appropriate Node attribute which takes a Node of the same
Node Type as NEW_PARENT; if one can not be found, the undefined value is
returned.  By default, the current value of the found attribute is ignored; but
if the optional argument ONLY_NOT_VALUED is true, then an otherwise acceptible
attribute name will not be returned if it already has a value.

=head2 get_container()

	my $model = $node->get_container();

This "getter" method returns the Container object which this Node lives in, if
any.

=head2 put_in_container( NEW_CONTAINER )

This "setter" method will put the current Node into the Container given as the
NEW_CONTAINER argument if it can, which moves the Node from "Alone" to "At
Home" status.

=head2 take_from_container()

This "setter" method will take the current Node from its Container if it can,
which moves the Node from "At Home" to "Alone" status.

=head2 are_reciprocal_links()

This "getter" method returns a true boolean value if the current Node is in
"Well Known" status, and false otherwise.

=head2 add_reciprocal_links()

This "setter" method will move the current Node from "At Home" to "Well Known"
status if possible.

=head2 remove_reciprocal_links()

This "setter" method will move the current Node from "Well Known" to "At Home"
status if possible.

=head2 move_before_sibling( SIBLING[, PARENT] )

This "setter" method allows you to change the order of child Nodes under a
common parent Node; specifically, it moves the current Node to a position just
above/before the sibling Node specified in the SIBLING Node ref argument, if it
can.  You can only invoke it on a Node that is "Well Known", since that is the
only time it exists in its parent's child list at all.  Since a Node can have
multiple parent Nodes (and the sibling likewise), the optional PARENT argument
lets you specify which parent's child list you want to move in; if you do not
provide an PARENT value, then the current Node's primary parent Node is used,
if possible.  This method will throw an exception if the current Node and the
specified sibling or parent Nodes are not appropriately related to each other
(parent <-> child).  If you want to move the current Node to follow the sibling
instead, then invoke this method on the sibling.

=head2 get_child_nodes([ NODE_TYPE ])

	my $ra_node_list = $node->get_child_nodes();
	my $ra_node_list = $node->get_child_nodes( 'table' );

This "getter" method returns a list of this object's child Nodes, in a new
array ref. If the optional argument NODE_TYPE is defined, then only child Nodes
of that Node Type are returned; otherwise, all child Nodes are returned.  All
Nodes are returned in the same order they were added.

=head2 add_child_node( NEW_CHILD )

	$node->add_child_node( $child );

This "setter" method allows you to add a new child Node to this object, which
is provided as the single NEW_CHILD Node ref argument.  The new child Node is
appended to the list of existing child Nodes, and the current Node becomes the
new or first primary parent Node of NEW_CHILD.

=head2 add_child_nodes( LIST )

	$model->add_child_nodes( [$child1,$child2] );
	$model->add_child_nodes( $child );

This "setter" method takes an array ref in its single LIST argument, and calls
add_child_node() for each element found in it.

=head2 test_deferrable_constraints()

This "getter" method implements several types of deferrable data validation, to
make sure that this Node is ready to be used; it throws an exception if it can
find anything wrong.  This method can be used on any Node regardless of its
current node evolution state, but that state does affect which tests are
performed; "Well Known" Nodes get all the tests, while "Alone" Nodes skip some.

=head1 CONTAINER OR NODE METHODS FOR DEBUGGING

The following 3 "getter" methods can be invoked either on Container or Node
objects, and will return a tree-arranged structure having the contents of a
Node and all its children (to the Nth generation).  The previous statement
assumes that all the 'children' have a true are_reciprocal_links property,
which means that a Node's parent is aware of it; if that property is false for
a Node, the assumption is that said Node is still being constructed, and
neither it nor its children will be included in the output.  If you invoke the
3 methods on a Node, then that Node will be the root of the returned structure.
If you invoke them on a Container, then a few pseudo-nodes will be output with
all the normal Nodes in the Container as their children.

=head2 get_all_properties()

	$rh_node_properties = $node->get_all_properties();
	$rh_node_properties = $container->get_all_properties();

This method returns a deep copy of all of the properties of this object as
non-blessed Perl data structures.  These data structures are also arranged in a
tree, but they do not have any circular references.  The main purpose,
currently, of get_all_properties() is to make it easier to debug or test this
class; it makes it easier to see at a glance whether the other class methods
are doing what you expect.  The output of this method should also be easy to
serialize or unserialize to strings of Perl code or xml or other things, should
you want to compare your results easily by string compare (see
"get_all_properties_as_perl_str()" and "get_all_properties_as_xml_str()").

=head2 get_all_properties_as_perl_str([ NO_INDENTS ])

	$perl_code_str = $container->get_all_properties_as_perl_str();
	$perl_code_str = $container->get_all_properties_as_perl_str( 1 );
	$perl_code_str = $node->get_all_properties_as_perl_str();
	$perl_code_str = $node->get_all_properties_as_perl_str( 1 );

This method is a wrapper for get_all_properties() that serializes its output
into a pretty-printed string of Perl code, suitable for humans to read.  You
should be able to eval this string and produce the original structure.  By
default, contents of lists are indented under the lists they are in (easier to
read); if the optional boolean argument NO_INDENTS is true, then all output
lines will be flush with the left, saving a fair amount of memory in what the
resulting string consumes.  (That said, even the indents are tabs, which take
up much less space than multiple spaces per indent level.)

=head2 get_all_properties_as_xml_str([ NO_INDENTS ])

	$xml_doc_str = $container->get_all_properties_as_xml_str();
	$xml_doc_str = $container->get_all_properties_as_xml_str( 1 );
	$xml_doc_str = $node->get_all_properties_as_xml_str();
	$xml_doc_str = $node->get_all_properties_as_xml_str( 1 );

This method is a wrapper for get_all_properties() that serializes its output
into a pretty-printed string of XML, suitable for humans to read. By default,
child nodes are indented under their parent nodes (easier to read); if the
optional boolean argument NO_INDENTS is true, then all output lines will be
flush with the left, saving a fair amount of memory in what the resulting
string consumes.  (That said, even the indents are tabs, which take up much
less space than multiple spaces per indent level.)

=head1 INFORMATION FUNCTIONS AND METHODS

These "getter" functions/methods are all intended for use by programs that want
to dynamically interface with SQL::SyntaxModel, especially those programs that
will generate a user interface for manual editing of data stored in or accessed
through SQL::SyntaxModel constructs.  It will allow such programs to continue
working without many changes while SQL::SyntaxModel itself continues to evolve.
In a manner of speaking, these functions/methods let a caller program query as
to what 'schema' or 'business logic' drive this class.  These functions/methods
are all deterministic and stateless; they can be used in any context and will
always give the same answers from the same arguments, and no object properties
are used.  You can invoke them from any kind of object that SQL::SyntaxModel
implements, or straight off of the class name itself, like a 'static' method.  
All of these functions return the undefined value if they match nothing.

=head2 valid_enumerated_types([ ENUM_TYPE ])

This function by default returns a list of the valid enumerated types that
SQL::SyntaxModel recognizes; if the optional ENUM_TYPE argument is given, it
just returns true if that matches a valid type, and false otherwise.

=head2 valid_enumerated_type_values( ENUM_TYPE[, ENUM_VALUE] )

This function by default returns a list of the values that SQL::SyntaxModel
recognizes for the enumerated type given in the ENUM_TYPE argument; if the
optional ENUM_VALUE argument is given, it just returns true if that matches an
allowed value, and false otherwise.

=head2 valid_node_types([ NODE_TYPE ])

This function by default returns a list of the valid Node Types that
SQL::SyntaxModel recognizes; if the optional NODE_TYPE argument is given, it
just returns true if that matches a valid type, and false otherwise.

=head2 valid_node_type_literal_attributes( NODE_TYPE[, ATTR_NAME] )

This function by default returns a Hash ref where the keys are the names of the
literal attributes that SQL::SyntaxModel recognizes for the Node Type given in
the NODE_TYPE argument, and where the values are the literal data types that
values for those attributes must be; if the optional ATTR_NAME argument is
given, it just returns the literal data type for the named attribute.

=head2 valid_node_type_enumerated_attributes( NODE_TYPE[, ATTR_NAME] )

This function by default returns a Hash ref where the keys are the names of the
enumerated attributes that SQL::SyntaxModel recognizes for the Node Type given
in the NODE_TYPE argument, and where the values are the enumerated data types
that values for those attributes must be; if the optional ATTR_NAME argument is
given, it just returns the enumerated data type for the named attribute.

=head2 valid_node_type_node_ref_attributes( NODE_TYPE[, ATTR_NAME] )

This function by default returns a Hash ref where the keys are the names of the
node attributes that SQL::SyntaxModel recognizes for the Node Type given in the
NODE_TYPE argument, and where the values are the Node Types that values for
those attributes must be; if the optional ATTR_NAME argument is given, it just
returns the Node Type for the named attribute.

=head2 major_type_of_node_type_attribute( NODE_TYPE, ATTR_NAME )

This "getter" function returns the major type for the attribute of NODE_TYPE
Nodes named ATTR_NAME, which is one of 'ID', 'LITERAL', 'ENUM' or 'NODE'.

=head2 valid_node_type_parent_attribute_names( NODE_TYPE[, ATTR_NAME] )

This function by default returns an Array ref which lists the names of the node
attributes that are allowed to reference the primary parent of a Node whose
type is specified in the NODE_TYPE argument; if the optional ATTR_NAME argument
is given, it just returns true the named attribute may reference the primary
parent of a NODE_TYPE Node.

=head2 node_types_with_pseudonode_parents([ NODE_TYPE ])

This function by default returns a Hash ref where the keys are the names of the
Node Types whose primary parents can only be pseudo-nodes, and where the values
name the pseudo-nodes they are the children of; if the optional NODE_TYPE
argument is given, it just returns the pseudo-node for that Node Type.

=head2 mandatory_node_type_literal_attribute_names( NODE_TYPE[, ATTR_NAME] )

This function by default returns an Array ref which lists the always-mandatory
literal attributes of the Node Type specified in the NODE_TYPE argument; if the
optional ATR_NAME argument is given, it just returns true if that attribute is
always-mandatory.

=head2 mandatory_node_type_enumerated_attribute_names( NODE_TYPE[, ATTR_NAME] )

This function by default returns an Array ref which lists the always-mandatory
enumerated attributes of the Node Type specified in the NODE_TYPE argument; if
the optional ATR_NAME argument is given, it just returns true if that attribute
is always-mandatory.

=head2 mandatory_node_type_node_ref_attribute_names( NODE_TYPE[, ATTR_NAME] )

This function by default returns an Array ref which lists the always-mandatory
node ref attributes of the Node Type specified in the NODE_TYPE argument; if
the optional ATR_NAME argument is given, it just returns true if that attribute
is always-mandatory.

=head1 BUGS

This module is currently in pre-alpha development status, meaning that some
parts of it will be changed in the near future, perhaps in incompatible ways;
however, I believe that the largest short-term changes are already done.  This
module will indeed execute and do a variety of things, but it isn't yet
recommended for any kind of serious use.  The current state is analagous to
'developer releases' of operating systems; you can study it with the intent of
using it in the future, but you should hold off writing any volume of code
against it which you aren't prepared to rewrite later as the API changes.  Also,
the module hasn't been tested as much as I would like, but it has tested the
more commonly used areas.  All of the code included with the other modules that
sub-class this one has been executed, which tests most internal functions and
data.  All of this said, I plan to move this module into alpha development
status within the next few releases, once I start using it in a production
environment myself.

=head1 CAVEATS

All SQL::SyntaxModel::Container objects contain circular references by design
(or more specifically, when 1 or more Node is in one).  When you are done with
a Container object, you should explicitly call its "destroy()" method prior to
letting your references to it go out of scope, or you will leak the memory it
used.  I<Up to and including SQL::SyntaxModel's 2004-03-03 release I had
implemented a cludge that wrapped the actual Container object in a second
object that would automatically destroy its contents when it went out of scope.
 While that saved users from doing manual destruction, it introduced
potentially worse problems, such as the Container being destroyed too early
(and it added complexity regardless); as of 2004-03-08 I did away with the
cludge.>

=head1 SEE ALSO

perl(1), SQL::SyntaxModel::L::en, SQL::SyntaxModel::Language,
SQL::SyntaxModel::API_C, Locale::KeyedText, Rosetta,
Rosetta::Utility::SQLBuilder, Rosetta::Engine::Generic,
SQL::SyntaxModel::ByTree, SQL::SyntaxModel::SkipID, DBI, SQL::Statement,
SQL::Translator, SQL::YASP, SQL::Generator, SQL::Schema, SQL::Abstract,
SQL::Snippet, SQL::Catalog, DB::Ent, DBIx::Abstract, DBIx::AnyDBD,
DBIx::DBSchema, DBIx::Namespace, DBIx::SearchBuilder, TripleStore, and various
other modules.

=cut
