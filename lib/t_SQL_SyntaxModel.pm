# This module contains sample input and output data which is used to test 
# SQL::SyntaxModel, and possibly other modules that are derived from it.

package # hide this class name from PAUSE indexer
t_SQL_SyntaxModel;
use strict;
use warnings;

######################################################################

sub make_a_node {
	my ($node_type, $node_id, $model) = @_;
	my $node = $model->new_node( $node_type );
	$node->set_node_id( $node_id );
	$node->put_in_container( $model );
	$node->add_reciprocal_links();
	return( $node );
}

sub make_a_child_node {
	my ($node_type, $node_id, $pp_node, $pp_attr) = @_;
	my $node = $pp_node->new_node( $node_type );
	$node->set_node_id( $node_id );
	$node->put_in_container( $pp_node->get_container() );
	$node->add_reciprocal_links();
	$node->set_node_ref_attribute( $pp_attr, $pp_node );
	$node->set_parent_node_attribute_name( $pp_attr );
	return( $node );
}

sub create_and_populate_model {
	my (undef, $class) = @_;

	my $model = $class->new_container();

	##### FIRST SET ELEMENT-TYPE DETAILS #####

	# Create user-defined data type domain that our database record primary keys are:
	my $dom_entity_id = make_a_node( 'domain', 1, $model );
	$dom_entity_id->set_literal_attribute( 'name', 'entity_id' );
	$dom_entity_id->set_enumerated_attribute( 'base_type', 'NUM_INT' );
	$dom_entity_id->set_literal_attribute( 'num_scale', 9 );

	# Create user-defined data type domain that our person names are:
	my $dom_pers_name = make_a_node( 'domain', 2, $model );
	$dom_pers_name->set_literal_attribute( 'name', 'person_name' );
	$dom_pers_name->set_enumerated_attribute( 'base_type', 'STR_CHAR' );
	$dom_pers_name->set_literal_attribute( 'max_chars', 100 );

	##### NEXT SET CATALOG BLUEPRINT-TYPE DETAILS #####

	# Describe the database catalog blueprint that we will store our data in:
	my $catalog_bp = make_a_node( 'catalog', 1, $model );

	# Define the unrealized database user that owns our primary schema:
	my $owner = make_a_child_node( 'owner', 1, $catalog_bp, 'catalog' );

	# Define the primary schema that holds our data:
	my $schema = make_a_child_node( 'schema', 1, $catalog_bp, 'catalog' );
	$schema->set_literal_attribute( 'name', 'gene' );
	$schema->set_node_ref_attribute( 'owner', $owner );

	# Define the table that holds our data:
	my $tb_person = make_a_child_node( 'table', 1, $schema, 'schema' );
	$tb_person->set_literal_attribute( 'name', 'person' );

	# Define the 'person id' column of that table:
	my $tbc_person_id = make_a_child_node( 'table_col', 1, $tb_person, 'table' );
	$tbc_person_id->set_literal_attribute( 'name', 'person_id' );
	$tbc_person_id->set_node_ref_attribute( 'domain', $dom_entity_id );
	$tbc_person_id->set_literal_attribute( 'mandatory', 1 );
	$tbc_person_id->set_literal_attribute( 'default_val', 1 );
	$tbc_person_id->set_literal_attribute( 'auto_inc', 1 );

	# Define the 'person name' column of that table:
	my $tbc_person_name = make_a_child_node( 'table_col', 2, $tb_person, 'table' );
	$tbc_person_name->set_literal_attribute( 'name', 'name' );
	$tbc_person_name->set_node_ref_attribute( 'domain', $dom_pers_name );
	$tbc_person_name->set_literal_attribute( 'mandatory', 1 );

	# Define the 'father' column of that table:
	my $tbc_father_id = make_a_child_node( 'table_col', 3, $tb_person, 'table' );
	$tbc_father_id->set_literal_attribute( 'name', 'father_id' );
	$tbc_father_id->set_node_ref_attribute( 'domain', $dom_entity_id );
	$tbc_father_id->set_literal_attribute( 'mandatory', 0 );

	# Define the 'mother column of that table:
	my $tbc_mother_id = make_a_child_node( 'table_col', 4, $tb_person, 'table' );
	$tbc_mother_id->set_literal_attribute( 'name', 'mother_id' );
	$tbc_mother_id->set_node_ref_attribute( 'domain', $dom_entity_id );
	$tbc_mother_id->set_literal_attribute( 'mandatory', 0 );

	# Define the table primary key constraint on person.person_id:
	my $ipk_person = make_a_child_node( 'table_ind', 1, $tb_person, 'table' );
	$ipk_person->set_literal_attribute( 'name', 'primary' );
	$ipk_person->set_enumerated_attribute( 'ind_type', 'UNIQUE' );
	my $icpk_person = make_a_child_node( 'table_ind_col', 1, $ipk_person, 'table_ind' );
	$icpk_person->set_node_ref_attribute( 'table_col', $tbc_person_id );

	# Define a table foreign key constraint on person.father_id to person.person_id:
	my $ifk_father = make_a_child_node( 'table_ind', 2, $tb_person, 'table' );
	$ifk_father->set_literal_attribute( 'name', 'fk_father' );
	$ifk_father->set_enumerated_attribute( 'ind_type', 'FOREIGN' );
	$ifk_father->set_node_ref_attribute( 'f_table', $tb_person );
	my $icfk_father = make_a_child_node( 'table_ind_col', 2, $ifk_father, 'table_ind' );
	$icfk_father->set_node_ref_attribute( 'table_col', $tbc_father_id );
	$icfk_father->set_node_ref_attribute( 'f_table_col', $tbc_person_id );

	# Define a table foreign key constraint on person.mother_id to person.person_id:
	my $ifk_mother = make_a_child_node( 'table_ind', 3, $tb_person, 'table' );
	$ifk_mother->set_literal_attribute( 'name', 'fk_mother' );
	$ifk_mother->set_enumerated_attribute( 'ind_type', 'FOREIGN' );
	$ifk_mother->set_node_ref_attribute( 'f_table', $tb_person );
	my $icfk_mother = make_a_child_node( 'table_ind_col', 3, $ifk_mother, 'table_ind' );
	$icfk_mother->set_node_ref_attribute( 'table_col', $tbc_mother_id );
	$icfk_mother->set_node_ref_attribute( 'f_table_col', $tbc_person_id );

	##### NEXT SET APPLICATION BLUEPRINT-TYPE DETAILS #####

	# Describe a utility application for managing our database schema:
	my $setup_app = make_a_node( 'application', 1, $model );
	$setup_app->set_literal_attribute( 'name', 'Setup' );

	# Describe the data link that the utility app will use to talk to the database:
	my $setup_app_cl = make_a_child_node( 'catalog_link', 1, $setup_app, 'application' );
	$setup_app_cl->set_literal_attribute( 'name', 'admin_link' );
	$setup_app_cl->set_node_ref_attribute( 'target', $catalog_bp );

	# Describe a command (pseudo-routine) for setting up a database with our schema:
	my $cmd_install = make_a_child_node( 'command', 1, $setup_app, 'application' );
	$cmd_install->set_literal_attribute( 'name', 'install_app_schema' );
	$cmd_install->set_enumerated_attribute( 'command_type', 'DB_CREATE' );
	$cmd_install->set_literal_attribute( 'command_arg', '1' ); # id of 'catalog' Node

	# Describe a command (pseudo-routine) for tearing down a database with our schema:
	my $cmd_remove = make_a_child_node( 'command', 2, $setup_app, 'application' );
	$cmd_remove->set_literal_attribute( 'name', 'remove_app_schema' );
	$cmd_remove->set_enumerated_attribute( 'command_type', 'DB_DELETE' );
	$cmd_remove->set_literal_attribute( 'command_arg', '1' ); # id of 'catalog' Node

	# Describe a 'normal' application for viewing and editing database records:
	my $editor_app = make_a_node( 'application', 2, $model );
	$editor_app->set_literal_attribute( 'name', 'People Watcher' );

	# Describe the data link that the normal app will use to talk to the database:
	my $editor_app_cl = make_a_child_node( 'catalog_link', 2, $editor_app, 'application' );
	$editor_app_cl->set_literal_attribute( 'name', 'editor_link' );
	$editor_app_cl->set_node_ref_attribute( 'target', $catalog_bp );

	# Describe a routine that returns a cursor to fetch all records in the 'person' table:
	my $rt_fetchall = make_a_child_node( 'routine', 1, $editor_app, 'application' );
	$rt_fetchall->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_fetchall->set_literal_attribute( 'name', 'fetch_all_persons' );
	$rt_fetchall->set_enumerated_attribute( 'return_var_type', 'CURSOR' );
	my $vw_fetchall = make_a_child_node( 'view', 2, $rt_fetchall, 'routine' );
	$vw_fetchall->set_enumerated_attribute( 'view_context', 'ROUTINE' );
	$vw_fetchall->set_enumerated_attribute( 'view_type', 'MATCH' );
	$vw_fetchall->set_literal_attribute( 'match_all_cols', 1 );
	my $vw_fetchall_s1 = make_a_child_node( 'view_src', 3, $vw_fetchall, 'view' );
	$vw_fetchall_s1->set_literal_attribute( 'name', 'person' );
	$vw_fetchall_s1->set_node_ref_attribute( 'match_table', $tb_person );
	my $rtv_fet_cursor = make_a_child_node( 'routine_var', 4, $rt_fetchall, 'routine' );
	$rtv_fet_cursor->set_literal_attribute( 'name', 'person_cursor' );
	$rtv_fet_cursor->set_enumerated_attribute( 'var_type', 'CURSOR' );
	$rtv_fet_cursor->set_node_ref_attribute( 'curs_view', $vw_fetchall );
	my $rts_fet_open_c = make_a_child_node( 'routine_stmt', 5, $rt_fetchall, 'routine' );
	$rts_fet_open_c->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_fet_open_c->set_enumerated_attribute( 'call_sproc', 'CURSOR_OPEN' );
	my $rte_fet_open_c_a1 = make_a_child_node( 'routine_expr', 6, $rts_fet_open_c, 'p_stmt' );
	$rte_fet_open_c_a1->set_enumerated_attribute( 'expr_type', 'VAR' );
	$rte_fet_open_c_a1->set_node_ref_attribute( 'src_var', $rtv_fet_cursor );
	my $rts_fet_return = make_a_child_node( 'routine_stmt', 7, $rt_fetchall, 'routine' );
	$rts_fet_return->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_fet_return->set_enumerated_attribute( 'call_sproc', 'RETURN' );
	my $rte_fet_return_a1 = make_a_child_node( 'routine_expr', 8, $rts_fet_return, 'p_stmt' );
	$rte_fet_return_a1->set_enumerated_attribute( 'expr_type', 'VAR' );
	$rte_fet_return_a1->set_node_ref_attribute( 'src_var', $rtv_fet_cursor );
	# ... The calling code would then fetch whatever rows they want and then close the cursor

	# Describe a routine that inserts a record into the 'person' table:
	my $rt_insertone = make_a_child_node( 'routine', 9, $editor_app, 'application' );
	$rt_insertone->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_insertone->set_literal_attribute( 'name', 'insert_a_person' );
	my $rta_ins_pid = make_a_child_node( 'routine_arg', 10, $rt_insertone, 'routine' );
	$rta_ins_pid->set_literal_attribute( 'name', 'arg_person_id' );
	$rta_ins_pid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_pid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_ins_pnm = make_a_child_node( 'routine_arg', 11, $rt_insertone, 'routine' );
	$rta_ins_pnm->set_literal_attribute( 'name', 'arg_person_name' );
	$rta_ins_pnm->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_pnm->set_node_ref_attribute( 'domain', $dom_pers_name );
	my $rta_ins_fid = make_a_child_node( 'routine_arg', 12, $rt_insertone, 'routine' );
	$rta_ins_fid->set_literal_attribute( 'name', 'arg_father_id' );
	$rta_ins_fid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_fid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_ins_mid = make_a_child_node( 'routine_arg', 13, $rt_insertone, 'routine' );
	$rta_ins_mid->set_literal_attribute( 'name', 'arg_mother_id' );
	$rta_ins_mid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_mid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $vw_insertone = make_a_child_node( 'view', 14, $rt_insertone, 'routine' );
	$vw_insertone->set_enumerated_attribute( 'view_context', 'ROUTINE' );
	$vw_insertone->set_enumerated_attribute( 'view_type', 'MATCH' );
	my $vws_ins_pers = make_a_child_node( 'view_src', 15, $vw_insertone, 'view' );
	$vws_ins_pers->set_literal_attribute( 'name', 'person' );
	$vws_ins_pers->set_node_ref_attribute( 'match_table', $tb_person );
	my $vwsc_ins_pid = make_a_child_node( 'view_src_col', 16, $vws_ins_pers, 'src' );
	$vwsc_ins_pid->set_node_ref_attribute( 'match_table_col', $tbc_person_id );
	my $vwsc_ins_pnm = make_a_child_node( 'view_src_col', 17, $vws_ins_pers, 'src' );
	$vwsc_ins_pnm->set_node_ref_attribute( 'match_table_col', $tbc_person_name );
	my $vwsc_ins_fid = make_a_child_node( 'view_src_col', 18, $vws_ins_pers, 'src' );
	$vwsc_ins_fid->set_node_ref_attribute( 'match_table_col', $tbc_father_id );
	my $vwsc_ins_mid = make_a_child_node( 'view_src_col', 19, $vws_ins_pers, 'src' );
	$vwsc_ins_mid->set_node_ref_attribute( 'match_table_col', $tbc_mother_id );
	my $vwe_ins_set0 = make_a_child_node( 'view_expr', 20, $vw_insertone, 'view' );
	$vwe_ins_set0->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set0->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set0->set_node_ref_attribute( 'set_view_col', $vwsc_ins_pid );
	$vwe_ins_set0->set_node_ref_attribute( 'routine_arg', $rta_ins_pid );
	my $vwe_ins_set1 = make_a_child_node( 'view_expr', 21, $vw_insertone, 'view' );
	$vwe_ins_set1->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set1->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set1->set_node_ref_attribute( 'set_view_col', $vwsc_ins_pnm );
	$vwe_ins_set1->set_node_ref_attribute( 'routine_arg', $rta_ins_pnm );
	my $vwe_ins_set2 = make_a_child_node( 'view_expr', 22, $vw_insertone, 'view' );
	$vwe_ins_set2->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set2->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set2->set_node_ref_attribute( 'set_view_col', $vwsc_ins_fid );
	$vwe_ins_set2->set_node_ref_attribute( 'routine_arg', $rta_ins_fid );
	my $vwe_ins_set3 = make_a_child_node( 'view_expr', 23, $vw_insertone, 'view' );
	$vwe_ins_set3->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set3->set_node_ref_attribute( 'set_view_col', $vwsc_ins_mid );
	$vwe_ins_set3->set_node_ref_attribute( 'routine_arg', $rta_ins_mid );
	my $rts_insert = make_a_child_node( 'routine_stmt', 24, $rt_insertone, 'routine' );
	$rts_insert->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_insert->set_enumerated_attribute( 'call_sproc', 'INSERT' );
	$rts_insert->set_node_ref_attribute( 'c_view', $vw_insertone );
	# ... Currently, nothing is returned, though a count of affected rows could be later

	# Describe a routine that updates a record in the 'person' table:
	my $rt_updateone = make_a_child_node( 'routine', 25, $editor_app, 'application' );
	$rt_updateone->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_updateone->set_literal_attribute( 'name', 'update_a_person' );
	my $rta_upd_pid = make_a_child_node( 'routine_arg', 26, $rt_updateone, 'routine' );
	$rta_upd_pid->set_literal_attribute( 'name', 'arg_person_id' );
	$rta_upd_pid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_pid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_upd_pnm = make_a_child_node( 'routine_arg', 27, $rt_updateone, 'routine' );
	$rta_upd_pnm->set_literal_attribute( 'name', 'arg_person_name' );
	$rta_upd_pnm->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_pnm->set_node_ref_attribute( 'domain', $dom_pers_name );
	my $rta_upd_fid = make_a_child_node( 'routine_arg', 28, $rt_updateone, 'routine' );
	$rta_upd_fid->set_literal_attribute( 'name', 'arg_father_id' );
	$rta_upd_fid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_fid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_upd_mid = make_a_child_node( 'routine_arg', 29, $rt_updateone, 'routine' );
	$rta_upd_mid->set_literal_attribute( 'name', 'arg_mother_id' );
	$rta_upd_mid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_mid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $vw_updateone = make_a_child_node( 'view', 30, $rt_updateone, 'routine' );
	$vw_updateone->set_enumerated_attribute( 'view_context', 'ROUTINE' );
	$vw_updateone->set_enumerated_attribute( 'view_type', 'MATCH' );
	my $vws_upd_pers = make_a_child_node( 'view_src', 31, $vw_updateone, 'view' );
	$vws_upd_pers->set_literal_attribute( 'name', 'person' );
	$vws_upd_pers->set_node_ref_attribute( 'match_table', $tb_person );
	my $vwsc_upd_pid = make_a_child_node( 'view_src_col', 32, $vws_upd_pers, 'src' );
	$vwsc_upd_pid->set_node_ref_attribute( 'match_table_col', $tbc_person_id );
	my $vwsc_upd_pnm = make_a_child_node( 'view_src_col', 33, $vws_upd_pers, 'src' );
	$vwsc_upd_pnm->set_node_ref_attribute( 'match_table_col', $tbc_person_name );
	my $vwsc_upd_fid = make_a_child_node( 'view_src_col', 34, $vws_upd_pers, 'src' );
	$vwsc_upd_fid->set_node_ref_attribute( 'match_table_col', $tbc_father_id );
	my $vwsc_upd_mid = make_a_child_node( 'view_src_col', 35, $vws_upd_pers, 'src' );
	$vwsc_upd_mid->set_node_ref_attribute( 'match_table_col', $tbc_mother_id );
	my $vwe_upd_set1 = make_a_child_node( 'view_expr', 36, $vw_updateone, 'view' );
	$vwe_upd_set1->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_upd_set1->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_set1->set_node_ref_attribute( 'set_view_col', $vwsc_upd_pnm );
	$vwe_upd_set1->set_node_ref_attribute( 'routine_arg', $rta_upd_pnm );
	my $vwe_upd_set2 = make_a_child_node( 'view_expr', 37, $vw_updateone, 'view' );
	$vwe_upd_set2->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_upd_set2->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_set2->set_node_ref_attribute( 'set_view_col', $vwsc_upd_fid );
	$vwe_upd_set2->set_node_ref_attribute( 'routine_arg', $rta_upd_fid );
	my $vwe_upd_set3 = make_a_child_node( 'view_expr', 38, $vw_updateone, 'view' );
	$vwe_upd_set3->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_upd_set3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_set3->set_node_ref_attribute( 'set_view_col', $vwsc_upd_mid );
	$vwe_upd_set3->set_node_ref_attribute( 'routine_arg', $rta_upd_mid );
	my $vwe_upd_w1 = make_a_child_node( 'view_expr', 39, $vw_updateone, 'view' );
	$vwe_upd_w1->set_enumerated_attribute( 'view_part', 'WHERE' );
	$vwe_upd_w1->set_enumerated_attribute( 'expr_type', 'SFUNC' );
	$vwe_upd_w1->set_enumerated_attribute( 'sfunc', 'EQ' );
	my $vwe_upd_w2 = make_a_child_node( 'view_expr', 40, $vwe_upd_w1, 'p_expr' );
	$vwe_upd_w2->set_enumerated_attribute( 'expr_type', 'COL' );
	$vwe_upd_w2->set_node_ref_attribute( 'src_col', $vwsc_upd_pid );
	my $vwe_upd_w3 = make_a_child_node( 'view_expr', 41, $vwe_upd_w1, 'p_expr' );
	$vwe_upd_w3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_w3->set_node_ref_attribute( 'routine_arg', $rta_upd_pid );
	my $rts_update = make_a_child_node( 'routine_stmt', 42, $rt_updateone, 'routine' );
	$rts_update->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_update->set_enumerated_attribute( 'call_sproc', 'UPDATE' );
	$rts_update->set_node_ref_attribute( 'c_view', $vw_updateone );
	# ... Currently, nothing is returned, though a count of affected rows could be later

	# Describe a routine that deletes a record from the 'person' table:
	my $rt_deleteone = make_a_child_node( 'routine', 43, $editor_app, 'application' );
	$rt_deleteone->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_deleteone->set_literal_attribute( 'name', 'delete_a_person' );
	my $rta_del_pid = make_a_child_node( 'routine_arg', 44, $rt_deleteone, 'routine' );
	$rta_del_pid->set_literal_attribute( 'name', 'arg_person_id' );
	$rta_del_pid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_del_pid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $vw_deleteone = make_a_child_node( 'view', 45, $rt_deleteone, 'routine' );
	$vw_deleteone->set_enumerated_attribute( 'view_context', 'ROUTINE' );
	$vw_deleteone->set_enumerated_attribute( 'view_type', 'MATCH' );
	my $vws_del_pers = make_a_child_node( 'view_src', 46, $vw_deleteone, 'view' );
	$vws_del_pers->set_literal_attribute( 'name', 'person' );
	$vws_del_pers->set_node_ref_attribute( 'match_table', $tb_person );
	my $vwsc_del_pid = make_a_child_node( 'view_src_col', 47, $vws_del_pers, 'src' );
	$vwsc_del_pid->set_node_ref_attribute( 'match_table_col', $tbc_person_id );
	my $vwe_del_w1 = make_a_child_node( 'view_expr', 48, $vw_deleteone, 'view' );
	$vwe_del_w1->set_enumerated_attribute( 'view_part', 'WHERE' );
	$vwe_del_w1->set_enumerated_attribute( 'expr_type', 'SFUNC' );
	$vwe_del_w1->set_enumerated_attribute( 'sfunc', 'EQ' );
	my $vwe_del_w2 = make_a_child_node( 'view_expr', 49, $vwe_del_w1, 'p_expr' );
	$vwe_del_w2->set_enumerated_attribute( 'expr_type', 'COL' );
	$vwe_del_w2->set_node_ref_attribute( 'src_col', $vwsc_del_pid );
	my $vwe_del_w3 = make_a_child_node( 'view_expr', 50, $vwe_del_w1, 'p_expr' );
	$vwe_del_w3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_del_w3->set_node_ref_attribute( 'routine_arg', $rta_del_pid );
	my $rts_delete = make_a_child_node( 'routine_stmt', 51, $rt_deleteone, 'routine' );
	$rts_delete->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_delete->set_enumerated_attribute( 'call_sproc', 'DELETE' );
	$rts_delete->set_node_ref_attribute( 'c_view', $vw_deleteone );
	# ... Currently, nothing is returned, though a count of affected rows could be later

	##### NEXT SET PRODUCT-TYPE DETAILS #####

	# Indicate one database product we will be using:
	my $dsp_sqlite = make_a_node( 'data_storage_product', 1, $model );
	$dsp_sqlite->set_literal_attribute( 'product_code', 'SQLite_2_8_12' );
	$dsp_sqlite->set_literal_attribute( 'is_file_based', 1 );

	# Indicate another database product we will be using:
	my $dsp_oracle = make_a_node( 'data_storage_product', 2, $model );
	$dsp_oracle->set_literal_attribute( 'product_code', 'Oracle_9_i' );
	$dsp_oracle->set_literal_attribute( 'is_network_svc', 1 );

	# Indicate the data link product we will be using:
	my $dlp_odbc = make_a_node( 'data_link_product', 1, $model );
	$dlp_odbc->set_literal_attribute( 'product_code', 'ODBC' );

	##### NEXT SET 'TEST' INSTANCE-TYPE DETAILS #####

	# Define the database catalog instance that our testers will log-in to:
	my $test_db = make_a_node( 'catalog_instance', 1, $model );
	$test_db->set_node_ref_attribute( 'product', $dsp_sqlite );
	$test_db->set_node_ref_attribute( 'blueprint', $catalog_bp );
	$test_db->set_literal_attribute( 'name', 'test' );

	# Define the database user that owns the testing db schema:
	my $ownerI1 = make_a_child_node( 'user', 1, $test_db, 'catalog' );
	$ownerI1->set_enumerated_attribute( 'user_type', 'SCHEMA_OWNER' );
	$ownerI1->set_node_ref_attribute( 'match_owner', $owner );
	$ownerI1->set_literal_attribute( 'name', 'ronsealy' );
	$ownerI1->set_literal_attribute( 'password', 'K34dsD' );

	# Define a 'normal' database user that will work with the testing database:
	my $tester = make_a_child_node( 'user', 2, $test_db, 'catalog' );
	$tester->set_enumerated_attribute( 'user_type', 'DATA_EDITOR' );
	$tester->set_literal_attribute( 'name', 'joesmith' );
	$tester->set_literal_attribute( 'password', 'fdsKJ4' );

	# Define a utility app instance that testers will demonstrate with:
	my $test_setup_app = make_a_node( 'application_instance', 1, $model );
	$test_setup_app->set_node_ref_attribute( 'blueprint', $setup_app );
	$test_setup_app->set_literal_attribute( 'name', 'test Setup' );
	# Describe the data link instance that the utility app will use to talk to the test database:
	my $test_setup_app_cl = make_a_child_node( 'catalog_link_instance', 1, $test_setup_app, 'application' );
	$test_setup_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$test_setup_app_cl->set_node_ref_attribute( 'unrealized', $setup_app_cl );
	$test_setup_app_cl->set_node_ref_attribute( 'target', $test_db );
	$test_setup_app_cl->set_literal_attribute( 'local_dsn', 'test' );

	# Define a normal app instance that testers will demonstrate with:
	my $test_editor_app = make_a_node( 'application_instance', 2, $model );
	$test_editor_app->set_node_ref_attribute( 'blueprint', $editor_app );
	$test_editor_app->set_literal_attribute( 'name', 'test People Watcher' );
	# Describe the data link instance that the normal app will use to talk to the test database:
	my $test_editor_app_cl = make_a_child_node( 'catalog_link_instance', 2, $test_editor_app, 'application' );
	$test_editor_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$test_editor_app_cl->set_node_ref_attribute( 'unrealized', $editor_app_cl );
	$test_editor_app_cl->set_node_ref_attribute( 'target', $test_db );
	$test_editor_app_cl->set_literal_attribute( 'local_dsn', 'test' );

	##### NEXT SET 'DEMO' INSTANCE-TYPE DETAILS #####

	# Define the database catalog instance that marketers will demonstrate with:
	my $demo_db = make_a_node( 'catalog_instance', 2, $model );
	$demo_db->set_node_ref_attribute( 'product', $dsp_oracle );
	$demo_db->set_node_ref_attribute( 'blueprint', $catalog_bp );
	$demo_db->set_literal_attribute( 'name', 'demo' );

	# Define the database user that owns the demo db schema:
	my $ownerI2 = make_a_child_node( 'user', 3, $demo_db, 'catalog' );
	$ownerI2->set_enumerated_attribute( 'user_type', 'SCHEMA_OWNER' );
	$ownerI2->set_node_ref_attribute( 'match_owner', $owner );
	$ownerI2->set_literal_attribute( 'name', 'florence' );
	$ownerI2->set_literal_attribute( 'password', '0sfs8G' );

	# Define a 'normal' user that will work with the demo db:
	my $marketer = make_a_child_node( 'user', 4, $demo_db, 'catalog' );
	$marketer->set_enumerated_attribute( 'user_type', 'DATA_EDITOR' );
	$marketer->set_literal_attribute( 'name', 'thainuff' );
	$marketer->set_literal_attribute( 'password', '9340sd' );

	# Define a utility app instance that marketers will demonstrate with:
	my $demo_setup_app = make_a_node( 'application_instance', 3, $model );
	$demo_setup_app->set_node_ref_attribute( 'blueprint', $setup_app );
	$demo_setup_app->set_literal_attribute( 'name', 'demo Setup' );
	# Describe the data link instance that the utility app will use to talk to the demo database:
	my $demo_setup_app_cl = make_a_child_node( 'catalog_link_instance', 3, $demo_setup_app, 'application' );
	$demo_setup_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$demo_setup_app_cl->set_node_ref_attribute( 'unrealized', $setup_app_cl );
	$demo_setup_app_cl->set_node_ref_attribute( 'target', $demo_db );
	$demo_setup_app_cl->set_literal_attribute( 'local_dsn', 'demo' );

	# Define a normal app instance that marketers will demonstrate with:
	my $demo_editor_app = make_a_node( 'application_instance', 4, $model );
	$demo_editor_app->set_node_ref_attribute( 'blueprint', $editor_app );
	$demo_editor_app->set_literal_attribute( 'name', 'demo People Watcher' );
	# Describe the data link instance that the normal app will use to talk to the demo database:
	my $demo_editor_app_cl = make_a_child_node( 'catalog_link_instance', 4, $demo_editor_app, 'application' );
	$demo_editor_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$demo_editor_app_cl->set_node_ref_attribute( 'unrealized', $editor_app_cl );
	$demo_editor_app_cl->set_node_ref_attribute( 'target', $demo_db );
	$demo_editor_app_cl->set_literal_attribute( 'local_dsn', 'demo' );

	# ... we are still missing a bunch of things in this example ...

	# Now check that we didn't omit something important:
	$model->with_all_nodes_test_mandatory_attributes();

	return( $model );
}

######################################################################

sub expected_model_xml_output {
	return(
'<root>
	<elements>
		<domain id="1" name="entity_id" base_type="NUM_INT" num_scale="9" />
		<domain id="2" name="person_name" base_type="STR_CHAR" max_chars="100" />
	</elements>
	<blueprints>
		<catalog id="1">
			<owner id="1" catalog="1" />
			<schema id="1" catalog="1" name="gene" owner="1">
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
			<command id="1" application="1" name="install_app_schema" command_type="DB_CREATE" command_arg="1" />
			<command id="2" application="1" name="remove_app_schema" command_type="DB_DELETE" command_arg="1" />
		</application>
		<application id="2" name="People Watcher">
			<catalog_link id="2" application="2" name="editor_link" target="1" />
			<routine id="1" routine_type="ANONYMOUS" application="2" name="fetch_all_persons" return_var_type="CURSOR">
				<view id="2" view_context="ROUTINE" view_type="MATCH" routine="1" match_all_cols="1">
					<view_src id="3" view="2" name="person" match_table="1" />
				</view>
				<routine_var id="4" routine="1" name="person_cursor" var_type="CURSOR" curs_view="2" />
				<routine_stmt id="5" routine="1" stmt_type="SPROC" call_sproc="CURSOR_OPEN">
					<routine_expr id="6" expr_type="VAR" p_stmt="5" src_var="4" />
				</routine_stmt>
				<routine_stmt id="7" routine="1" stmt_type="SPROC" call_sproc="RETURN">
					<routine_expr id="8" expr_type="VAR" p_stmt="7" src_var="4" />
				</routine_stmt>
			</routine>
			<routine id="9" routine_type="ANONYMOUS" application="2" name="insert_a_person">
				<routine_arg id="10" routine="9" name="arg_person_id" var_type="SCALAR" domain="1" />
				<routine_arg id="11" routine="9" name="arg_person_name" var_type="SCALAR" domain="2" />
				<routine_arg id="12" routine="9" name="arg_father_id" var_type="SCALAR" domain="1" />
				<routine_arg id="13" routine="9" name="arg_mother_id" var_type="SCALAR" domain="1" />
				<view id="14" view_context="ROUTINE" view_type="MATCH" routine="9">
					<view_src id="15" view="14" name="person" match_table="1">
						<view_src_col id="16" src="15" match_table_col="1" />
						<view_src_col id="17" src="15" match_table_col="2" />
						<view_src_col id="18" src="15" match_table_col="3" />
						<view_src_col id="19" src="15" match_table_col="4" />
					</view_src>
					<view_expr id="20" expr_type="ARG" view="14" view_part="SET" set_view_col="16" routine_arg="10" />
					<view_expr id="21" expr_type="ARG" view="14" view_part="SET" set_view_col="17" routine_arg="11" />
					<view_expr id="22" expr_type="ARG" view="14" view_part="SET" set_view_col="18" routine_arg="12" />
					<view_expr id="23" expr_type="ARG" view="14" view_part="SET" set_view_col="19" routine_arg="13" />
				</view>
				<routine_stmt id="24" routine="9" stmt_type="SPROC" call_sproc="INSERT" c_view="14" />
			</routine>
			<routine id="25" routine_type="ANONYMOUS" application="2" name="update_a_person">
				<routine_arg id="26" routine="25" name="arg_person_id" var_type="SCALAR" domain="1" />
				<routine_arg id="27" routine="25" name="arg_person_name" var_type="SCALAR" domain="2" />
				<routine_arg id="28" routine="25" name="arg_father_id" var_type="SCALAR" domain="1" />
				<routine_arg id="29" routine="25" name="arg_mother_id" var_type="SCALAR" domain="1" />
				<view id="30" view_context="ROUTINE" view_type="MATCH" routine="25">
					<view_src id="31" view="30" name="person" match_table="1">
						<view_src_col id="32" src="31" match_table_col="1" />
						<view_src_col id="33" src="31" match_table_col="2" />
						<view_src_col id="34" src="31" match_table_col="3" />
						<view_src_col id="35" src="31" match_table_col="4" />
					</view_src>
					<view_expr id="36" expr_type="ARG" view="30" view_part="SET" set_view_col="33" routine_arg="27" />
					<view_expr id="37" expr_type="ARG" view="30" view_part="SET" set_view_col="34" routine_arg="28" />
					<view_expr id="38" expr_type="ARG" view="30" view_part="SET" set_view_col="35" routine_arg="29" />
					<view_expr id="39" expr_type="SFUNC" view="30" view_part="WHERE">
						<view_expr id="40" expr_type="COL" p_expr="39" src_col="32" />
						<view_expr id="41" expr_type="ARG" p_expr="39" routine_arg="26" />
					</view_expr>
				</view>
				<routine_stmt id="42" routine="25" stmt_type="SPROC" call_sproc="UPDATE" c_view="30" />
			</routine>
			<routine id="43" routine_type="ANONYMOUS" application="2" name="delete_a_person">
				<routine_arg id="44" routine="43" name="arg_person_id" var_type="SCALAR" domain="1" />
				<view id="45" view_context="ROUTINE" view_type="MATCH" routine="43">
					<view_src id="46" view="45" name="person" match_table="1">
						<view_src_col id="47" src="46" match_table_col="1" />
					</view_src>
					<view_expr id="48" expr_type="SFUNC" view="45" view_part="WHERE">
						<view_expr id="49" expr_type="COL" p_expr="48" src_col="47" />
						<view_expr id="50" expr_type="ARG" p_expr="48" routine_arg="44" />
					</view_expr>
				</view>
				<routine_stmt id="51" routine="43" stmt_type="SPROC" call_sproc="DELETE" c_view="45" />
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
'
	);
}

######################################################################

1;
