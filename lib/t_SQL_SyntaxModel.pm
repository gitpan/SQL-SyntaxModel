# This module contains sample input and output data which is used to test 
# SQL::SyntaxModel, and possibly other modules that are derived from it.

package # hide this class name from PAUSE indexer
t_SQL_SyntaxModel;
use strict;
use warnings;

######################################################################

sub make_a_node {
	my ($node_type, $model) = @_;
	my $node = $model->new_node( $node_type );
	$node->set_node_id( $model->get_next_free_node_id( $node_type ) );
	$node->put_in_container( $model );
	$node->add_reciprocal_links();
	return( $node );
}

sub make_a_child_node {
	my ($node_type, $pp_node, $pp_attr) = @_;
	my $container = $pp_node->get_container();
	my $node = $pp_node->new_node( $node_type );
	$node->set_node_id( $container->get_next_free_node_id( $node_type ) );
	$node->put_in_container( $container );
	$node->add_reciprocal_links();
	$node->set_node_ref_attribute( $pp_attr, $pp_node );
	$node->set_parent_node_attribute_name( $pp_attr );
	return( $node );
}

sub create_and_populate_model {
	my (undef, $class) = @_;

	my $model = $class->new_container();

	##### FIRST SET ELEMENT-TYPE DETAILS #####

	# As of SQL::SyntaxModel's 2004-05-12 release there aren't any of these (but some 
	# may come later); the 'domain' Nodes now appear under BLUEPRINT-TYPE DETAILS.

	##### NEXT SET CATALOG BLUEPRINT-TYPE DETAILS #####

	# Describe the database catalog blueprint that we will store our data in:
	my $catalog_bp = make_a_node( 'catalog', $model );

	# Define the unrealized database user that owns our primary schema:
	my $owner = make_a_child_node( 'owner', $catalog_bp, 'catalog' );

	# Define the primary schema that holds our data:
	my $schema = make_a_child_node( 'schema', $catalog_bp, 'catalog' );
	$schema->set_literal_attribute( 'name', 'gene' );
	$schema->set_node_ref_attribute( 'owner', $owner );

	# Create user-defined data type domain that our database record primary keys are:
	my $dom_entity_id = make_a_child_node( 'domain', $schema, 'schema' );
	$dom_entity_id->set_literal_attribute( 'name', 'entity_id' );
	$dom_entity_id->set_enumerated_attribute( 'base_type', 'NUM_INT' );
	$dom_entity_id->set_literal_attribute( 'num_precision', 9 );

	# Create user-defined data type domain that our person names are:
	my $dom_pers_name = make_a_child_node( 'domain', $schema, 'schema' );
	$dom_pers_name->set_literal_attribute( 'name', 'person_name' );
	$dom_pers_name->set_enumerated_attribute( 'base_type', 'STR_CHAR' );
	$dom_pers_name->set_literal_attribute( 'max_chars', 100 );
	$dom_pers_name->set_enumerated_attribute( 'char_enc', 'UTF8' );

	# Define the table that holds our data:
	my $tb_person = make_a_child_node( 'table', $schema, 'schema' );
	$tb_person->set_literal_attribute( 'name', 'person' );

	# Define the 'person id' column of that table:
	my $tbc_person_id = make_a_child_node( 'table_col', $tb_person, 'table' );
	$tbc_person_id->set_literal_attribute( 'name', 'person_id' );
	$tbc_person_id->set_node_ref_attribute( 'domain', $dom_entity_id );
	$tbc_person_id->set_literal_attribute( 'mandatory', 1 );
	$tbc_person_id->set_literal_attribute( 'default_val', 1 );
	$tbc_person_id->set_literal_attribute( 'auto_inc', 1 );

	# Define the 'person name' column of that table:
	my $tbc_person_name = make_a_child_node( 'table_col', $tb_person, 'table' );
	$tbc_person_name->set_literal_attribute( 'name', 'name' );
	$tbc_person_name->set_node_ref_attribute( 'domain', $dom_pers_name );
	$tbc_person_name->set_literal_attribute( 'mandatory', 1 );

	# Define the 'father' column of that table:
	my $tbc_father_id = make_a_child_node( 'table_col', $tb_person, 'table' );
	$tbc_father_id->set_literal_attribute( 'name', 'father_id' );
	$tbc_father_id->set_node_ref_attribute( 'domain', $dom_entity_id );
	$tbc_father_id->set_literal_attribute( 'mandatory', 0 );

	# Define the 'mother column of that table:
	my $tbc_mother_id = make_a_child_node( 'table_col', $tb_person, 'table' );
	$tbc_mother_id->set_literal_attribute( 'name', 'mother_id' );
	$tbc_mother_id->set_node_ref_attribute( 'domain', $dom_entity_id );
	$tbc_mother_id->set_literal_attribute( 'mandatory', 0 );

	# Define the table primary key constraint on person.person_id:
	my $ipk_person = make_a_child_node( 'table_ind', $tb_person, 'table' );
	$ipk_person->set_literal_attribute( 'name', 'primary' );
	$ipk_person->set_enumerated_attribute( 'ind_type', 'UNIQUE' );
	my $icpk_person = make_a_child_node( 'table_ind_col', $ipk_person, 'table_ind' );
	$icpk_person->set_node_ref_attribute( 'table_col', $tbc_person_id );

	# Define a table foreign key constraint on person.father_id to person.person_id:
	my $ifk_father = make_a_child_node( 'table_ind', $tb_person, 'table' );
	$ifk_father->set_literal_attribute( 'name', 'fk_father' );
	$ifk_father->set_enumerated_attribute( 'ind_type', 'FOREIGN' );
	$ifk_father->set_node_ref_attribute( 'f_table', $tb_person );
	my $icfk_father = make_a_child_node( 'table_ind_col', $ifk_father, 'table_ind' );
	$icfk_father->set_node_ref_attribute( 'table_col', $tbc_father_id );
	$icfk_father->set_node_ref_attribute( 'f_table_col', $tbc_person_id );

	# Define a table foreign key constraint on person.mother_id to person.person_id:
	my $ifk_mother = make_a_child_node( 'table_ind', $tb_person, 'table' );
	$ifk_mother->set_literal_attribute( 'name', 'fk_mother' );
	$ifk_mother->set_enumerated_attribute( 'ind_type', 'FOREIGN' );
	$ifk_mother->set_node_ref_attribute( 'f_table', $tb_person );
	my $icfk_mother = make_a_child_node( 'table_ind_col', $ifk_mother, 'table_ind' );
	$icfk_mother->set_node_ref_attribute( 'table_col', $tbc_mother_id );
	$icfk_mother->set_node_ref_attribute( 'f_table_col', $tbc_person_id );

	##### NEXT SET APPLICATION BLUEPRINT-TYPE DETAILS #####

	# Describe a utility application for managing our database schema:
	my $setup_app = make_a_node( 'application', $model );
	$setup_app->set_literal_attribute( 'name', 'Setup' );

	# Describe the data link that the utility app will use to talk to the database:
	my $setup_app_cl = make_a_child_node( 'catalog_link', $setup_app, 'application' );
	$setup_app_cl->set_literal_attribute( 'name', 'admin_link' );
	$setup_app_cl->set_node_ref_attribute( 'target', $catalog_bp );

	# Describe a command (pseudo-routine) for setting up a database with our schema:
	my $cmd_install = make_a_child_node( 'command', $setup_app, 'application' );
	$cmd_install->set_literal_attribute( 'name', 'install_app_schema' );
	$cmd_install->set_enumerated_attribute( 'command_type', 'DB_CREATE' );
	$cmd_install->set_node_ref_attribute( 'command_arg', $catalog_bp );

	# Describe a command (pseudo-routine) for tearing down a database with our schema:
	my $cmd_remove = make_a_child_node( 'command', $setup_app, 'application' );
	$cmd_remove->set_literal_attribute( 'name', 'remove_app_schema' );
	$cmd_remove->set_enumerated_attribute( 'command_type', 'DB_DELETE' );
	$cmd_remove->set_node_ref_attribute( 'command_arg', $catalog_bp );

	# Describe a 'normal' application for viewing and editing database records:
	my $editor_app = make_a_node( 'application', $model );
	$editor_app->set_literal_attribute( 'name', 'People Watcher' );

	# Describe the data link that the normal app will use to talk to the database:
	my $editor_app_cl = make_a_child_node( 'catalog_link', $editor_app, 'application' );
	$editor_app_cl->set_literal_attribute( 'name', 'editor_link' );
	$editor_app_cl->set_node_ref_attribute( 'target', $catalog_bp );

	# Describe a routine that returns a cursor to fetch all records in the 'person' table:
	my $rt_fetchall = make_a_child_node( 'routine', $editor_app, 'application' );
	$rt_fetchall->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_fetchall->set_literal_attribute( 'name', 'fetch_all_persons' );
	$rt_fetchall->set_enumerated_attribute( 'return_var_type', 'CURSOR' );
	my $vw_fetchall = make_a_child_node( 'view', $rt_fetchall, 'routine' );
	$vw_fetchall->set_enumerated_attribute( 'view_type', 'MATCH' );
	$vw_fetchall->set_literal_attribute( 'name', 'fetch_all_persons' );
	$vw_fetchall->set_literal_attribute( 'match_all_cols', 1 );
	my $vw_fetchall_s1 = make_a_child_node( 'view_src', $vw_fetchall, 'view' );
	$vw_fetchall_s1->set_literal_attribute( 'name', 'person' );
	$vw_fetchall_s1->set_node_ref_attribute( 'match_table', $tb_person );
	my $rtv_fet_cursor = make_a_child_node( 'routine_var', $rt_fetchall, 'routine' );
	$rtv_fet_cursor->set_literal_attribute( 'name', 'person_cursor' );
	$rtv_fet_cursor->set_enumerated_attribute( 'var_type', 'CURSOR' );
	$rtv_fet_cursor->set_node_ref_attribute( 'curs_view', $vw_fetchall );
	my $rts_fet_open_c = make_a_child_node( 'routine_stmt', $rt_fetchall, 'routine' );
	$rts_fet_open_c->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_fet_open_c->set_enumerated_attribute( 'call_sproc', 'CURSOR_OPEN' );
	my $rte_fet_open_c_a1 = make_a_child_node( 'routine_expr', $rts_fet_open_c, 'p_stmt' );
	$rte_fet_open_c_a1->set_enumerated_attribute( 'expr_type', 'VAR' );
	$rte_fet_open_c_a1->set_node_ref_attribute( 'routine_var', $rtv_fet_cursor );
	my $rts_fet_return = make_a_child_node( 'routine_stmt', $rt_fetchall, 'routine' );
	$rts_fet_return->set_enumerated_attribute( 'stmt_type', 'RETURN' );
	my $rte_fet_return_a1 = make_a_child_node( 'routine_expr', $rts_fet_return, 'p_stmt' );
	$rte_fet_return_a1->set_enumerated_attribute( 'expr_type', 'VAR' );
	$rte_fet_return_a1->set_node_ref_attribute( 'routine_var', $rtv_fet_cursor );
	# ... The calling code would then fetch whatever rows they want and then close the cursor

	# Describe a routine that inserts a record into the 'person' table:
	my $rt_insertone = make_a_child_node( 'routine', $editor_app, 'application' );
	$rt_insertone->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_insertone->set_literal_attribute( 'name', 'insert_a_person' );
	my $rta_ins_pid = make_a_child_node( 'routine_arg', $rt_insertone, 'routine' );
	$rta_ins_pid->set_literal_attribute( 'name', 'arg_person_id' );
	$rta_ins_pid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_pid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_ins_pnm = make_a_child_node( 'routine_arg', $rt_insertone, 'routine' );
	$rta_ins_pnm->set_literal_attribute( 'name', 'arg_person_name' );
	$rta_ins_pnm->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_pnm->set_node_ref_attribute( 'domain', $dom_pers_name );
	my $rta_ins_fid = make_a_child_node( 'routine_arg', $rt_insertone, 'routine' );
	$rta_ins_fid->set_literal_attribute( 'name', 'arg_father_id' );
	$rta_ins_fid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_fid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_ins_mid = make_a_child_node( 'routine_arg', $rt_insertone, 'routine' );
	$rta_ins_mid->set_literal_attribute( 'name', 'arg_mother_id' );
	$rta_ins_mid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_ins_mid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $vw_insertone = make_a_child_node( 'view', $rt_insertone, 'routine' );
	$vw_insertone->set_enumerated_attribute( 'view_type', 'MATCH' );
	$vw_insertone->set_literal_attribute( 'name', 'insert_a_person' );
	my $vws_ins_pers = make_a_child_node( 'view_src', $vw_insertone, 'view' );
	$vws_ins_pers->set_literal_attribute( 'name', 'person' );
	$vws_ins_pers->set_node_ref_attribute( 'match_table', $tb_person );
	my $vwsc_ins_pid = make_a_child_node( 'view_src_col', $vws_ins_pers, 'src' );
	$vwsc_ins_pid->set_node_ref_attribute( 'match_table_col', $tbc_person_id );
	my $vwsc_ins_pnm = make_a_child_node( 'view_src_col', $vws_ins_pers, 'src' );
	$vwsc_ins_pnm->set_node_ref_attribute( 'match_table_col', $tbc_person_name );
	my $vwsc_ins_fid = make_a_child_node( 'view_src_col', $vws_ins_pers, 'src' );
	$vwsc_ins_fid->set_node_ref_attribute( 'match_table_col', $tbc_father_id );
	my $vwsc_ins_mid = make_a_child_node( 'view_src_col', $vws_ins_pers, 'src' );
	$vwsc_ins_mid->set_node_ref_attribute( 'match_table_col', $tbc_mother_id );
	my $vwe_ins_set0 = make_a_child_node( 'view_expr', $vw_insertone, 'view' );
	$vwe_ins_set0->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set0->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set0->set_node_ref_attribute( 'set_view_col', $vwsc_ins_pid );
	$vwe_ins_set0->set_node_ref_attribute( 'routine_arg', $rta_ins_pid );
	my $vwe_ins_set1 = make_a_child_node( 'view_expr', $vw_insertone, 'view' );
	$vwe_ins_set1->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set1->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set1->set_node_ref_attribute( 'set_view_col', $vwsc_ins_pnm );
	$vwe_ins_set1->set_node_ref_attribute( 'routine_arg', $rta_ins_pnm );
	my $vwe_ins_set2 = make_a_child_node( 'view_expr', $vw_insertone, 'view' );
	$vwe_ins_set2->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set2->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set2->set_node_ref_attribute( 'set_view_col', $vwsc_ins_fid );
	$vwe_ins_set2->set_node_ref_attribute( 'routine_arg', $rta_ins_fid );
	my $vwe_ins_set3 = make_a_child_node( 'view_expr', $vw_insertone, 'view' );
	$vwe_ins_set3->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_ins_set3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_ins_set3->set_node_ref_attribute( 'set_view_col', $vwsc_ins_mid );
	$vwe_ins_set3->set_node_ref_attribute( 'routine_arg', $rta_ins_mid );
	my $rts_insert = make_a_child_node( 'routine_stmt', $rt_insertone, 'routine' );
	$rts_insert->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_insert->set_enumerated_attribute( 'call_sproc', 'INSERT' );
	$rts_insert->set_node_ref_attribute( 'view_for_dml', $vw_insertone );
	# ... Currently, nothing is returned, though a count of affected rows could be later

	# Describe a routine that updates a record in the 'person' table:
	my $rt_updateone = make_a_child_node( 'routine', $editor_app, 'application' );
	$rt_updateone->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_updateone->set_literal_attribute( 'name', 'update_a_person' );
	my $rta_upd_pid = make_a_child_node( 'routine_arg', $rt_updateone, 'routine' );
	$rta_upd_pid->set_literal_attribute( 'name', 'arg_person_id' );
	$rta_upd_pid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_pid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_upd_pnm = make_a_child_node( 'routine_arg', $rt_updateone, 'routine' );
	$rta_upd_pnm->set_literal_attribute( 'name', 'arg_person_name' );
	$rta_upd_pnm->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_pnm->set_node_ref_attribute( 'domain', $dom_pers_name );
	my $rta_upd_fid = make_a_child_node( 'routine_arg', $rt_updateone, 'routine' );
	$rta_upd_fid->set_literal_attribute( 'name', 'arg_father_id' );
	$rta_upd_fid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_fid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $rta_upd_mid = make_a_child_node( 'routine_arg', $rt_updateone, 'routine' );
	$rta_upd_mid->set_literal_attribute( 'name', 'arg_mother_id' );
	$rta_upd_mid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_upd_mid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $vw_updateone = make_a_child_node( 'view', $rt_updateone, 'routine' );
	$vw_updateone->set_enumerated_attribute( 'view_type', 'MATCH' );
	$vw_updateone->set_literal_attribute( 'name', 'update_a_person' );
	my $vws_upd_pers = make_a_child_node( 'view_src', $vw_updateone, 'view' );
	$vws_upd_pers->set_literal_attribute( 'name', 'person' );
	$vws_upd_pers->set_node_ref_attribute( 'match_table', $tb_person );
	my $vwsc_upd_pid = make_a_child_node( 'view_src_col', $vws_upd_pers, 'src' );
	$vwsc_upd_pid->set_node_ref_attribute( 'match_table_col', $tbc_person_id );
	my $vwsc_upd_pnm = make_a_child_node( 'view_src_col', $vws_upd_pers, 'src' );
	$vwsc_upd_pnm->set_node_ref_attribute( 'match_table_col', $tbc_person_name );
	my $vwsc_upd_fid = make_a_child_node( 'view_src_col', $vws_upd_pers, 'src' );
	$vwsc_upd_fid->set_node_ref_attribute( 'match_table_col', $tbc_father_id );
	my $vwsc_upd_mid = make_a_child_node( 'view_src_col', $vws_upd_pers, 'src' );
	$vwsc_upd_mid->set_node_ref_attribute( 'match_table_col', $tbc_mother_id );
	my $vwe_upd_set1 = make_a_child_node( 'view_expr', $vw_updateone, 'view' );
	$vwe_upd_set1->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_upd_set1->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_set1->set_node_ref_attribute( 'set_view_col', $vwsc_upd_pnm );
	$vwe_upd_set1->set_node_ref_attribute( 'routine_arg', $rta_upd_pnm );
	my $vwe_upd_set2 = make_a_child_node( 'view_expr', $vw_updateone, 'view' );
	$vwe_upd_set2->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_upd_set2->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_set2->set_node_ref_attribute( 'set_view_col', $vwsc_upd_fid );
	$vwe_upd_set2->set_node_ref_attribute( 'routine_arg', $rta_upd_fid );
	my $vwe_upd_set3 = make_a_child_node( 'view_expr', $vw_updateone, 'view' );
	$vwe_upd_set3->set_enumerated_attribute( 'view_part', 'SET' );
	$vwe_upd_set3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_set3->set_node_ref_attribute( 'set_view_col', $vwsc_upd_mid );
	$vwe_upd_set3->set_node_ref_attribute( 'routine_arg', $rta_upd_mid );
	my $vwe_upd_w1 = make_a_child_node( 'view_expr', $vw_updateone, 'view' );
	$vwe_upd_w1->set_enumerated_attribute( 'view_part', 'WHERE' );
	$vwe_upd_w1->set_enumerated_attribute( 'expr_type', 'SFUNC' );
	$vwe_upd_w1->set_enumerated_attribute( 'call_sfunc', 'EQ' );
	my $vwe_upd_w2 = make_a_child_node( 'view_expr', $vwe_upd_w1, 'p_expr' );
	$vwe_upd_w2->set_enumerated_attribute( 'expr_type', 'COL' );
	$vwe_upd_w2->set_node_ref_attribute( 'src_col', $vwsc_upd_pid );
	my $vwe_upd_w3 = make_a_child_node( 'view_expr', $vwe_upd_w1, 'p_expr' );
	$vwe_upd_w3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_upd_w3->set_node_ref_attribute( 'routine_arg', $rta_upd_pid );
	my $rts_update = make_a_child_node( 'routine_stmt', $rt_updateone, 'routine' );
	$rts_update->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_update->set_enumerated_attribute( 'call_sproc', 'UPDATE' );
	$rts_update->set_node_ref_attribute( 'view_for_dml', $vw_updateone );
	# ... Currently, nothing is returned, though a count of affected rows could be later

	# Describe a routine that deletes a record from the 'person' table:
	my $rt_deleteone = make_a_child_node( 'routine', $editor_app, 'application' );
	$rt_deleteone->set_enumerated_attribute( 'routine_type', 'ANONYMOUS' );
	$rt_deleteone->set_literal_attribute( 'name', 'delete_a_person' );
	my $rta_del_pid = make_a_child_node( 'routine_arg', $rt_deleteone, 'routine' );
	$rta_del_pid->set_literal_attribute( 'name', 'arg_person_id' );
	$rta_del_pid->set_enumerated_attribute( 'var_type', 'SCALAR' );
	$rta_del_pid->set_node_ref_attribute( 'domain', $dom_entity_id );
	my $vw_deleteone = make_a_child_node( 'view', $rt_deleteone, 'routine' );
	$vw_deleteone->set_enumerated_attribute( 'view_type', 'MATCH' );
	$vw_deleteone->set_literal_attribute( 'name', 'delete_a_person' );
	my $vws_del_pers = make_a_child_node( 'view_src', $vw_deleteone, 'view' );
	$vws_del_pers->set_literal_attribute( 'name', 'person' );
	$vws_del_pers->set_node_ref_attribute( 'match_table', $tb_person );
	my $vwsc_del_pid = make_a_child_node( 'view_src_col', $vws_del_pers, 'src' );
	$vwsc_del_pid->set_node_ref_attribute( 'match_table_col', $tbc_person_id );
	my $vwe_del_w1 = make_a_child_node( 'view_expr', $vw_deleteone, 'view' );
	$vwe_del_w1->set_enumerated_attribute( 'view_part', 'WHERE' );
	$vwe_del_w1->set_enumerated_attribute( 'expr_type', 'SFUNC' );
	$vwe_del_w1->set_enumerated_attribute( 'call_sfunc', 'EQ' );
	my $vwe_del_w2 = make_a_child_node( 'view_expr', $vwe_del_w1, 'p_expr' );
	$vwe_del_w2->set_enumerated_attribute( 'expr_type', 'COL' );
	$vwe_del_w2->set_node_ref_attribute( 'src_col', $vwsc_del_pid );
	my $vwe_del_w3 = make_a_child_node( 'view_expr', $vwe_del_w1, 'p_expr' );
	$vwe_del_w3->set_enumerated_attribute( 'expr_type', 'ARG' );
	$vwe_del_w3->set_node_ref_attribute( 'routine_arg', $rta_del_pid );
	my $rts_delete = make_a_child_node( 'routine_stmt', $rt_deleteone, 'routine' );
	$rts_delete->set_enumerated_attribute( 'stmt_type', 'SPROC' );
	$rts_delete->set_enumerated_attribute( 'call_sproc', 'DELETE' );
	$rts_delete->set_node_ref_attribute( 'view_for_dml', $vw_deleteone );
	# ... Currently, nothing is returned, though a count of affected rows could be later

	##### NEXT SET PRODUCT-TYPE DETAILS #####

	# Indicate one database product we will be using:
	my $dsp_sqlite = make_a_node( 'data_storage_product', $model );
	$dsp_sqlite->set_literal_attribute( 'product_code', 'SQLite_2_8_12' );
	$dsp_sqlite->set_literal_attribute( 'is_file_based', 1 );

	# Indicate another database product we will be using:
	my $dsp_oracle = make_a_node( 'data_storage_product', $model );
	$dsp_oracle->set_literal_attribute( 'product_code', 'Oracle_9_i' );
	$dsp_oracle->set_literal_attribute( 'is_network_svc', 1 );

	# Indicate the data link product we will be using:
	my $dlp_odbc = make_a_node( 'data_link_product', $model );
	$dlp_odbc->set_literal_attribute( 'product_code', 'ODBC' );

	##### NEXT SET 'TEST' INSTANCE-TYPE DETAILS #####

	# Define the database catalog instance that our testers will log-in to:
	my $test_db = make_a_node( 'catalog_instance', $model );
	$test_db->set_node_ref_attribute( 'product', $dsp_sqlite );
	$test_db->set_node_ref_attribute( 'blueprint', $catalog_bp );
	$test_db->set_literal_attribute( 'name', 'test' );

	# Define the database user that owns the testing db schema:
	my $ownerI1 = make_a_child_node( 'user', $test_db, 'catalog' );
	$ownerI1->set_enumerated_attribute( 'user_type', 'SCHEMA_OWNER' );
	$ownerI1->set_node_ref_attribute( 'match_owner', $owner );
	$ownerI1->set_literal_attribute( 'name', 'ronsealy' );
	$ownerI1->set_literal_attribute( 'password', 'K34dsD' );

	# Define a 'normal' database user that will work with the testing database:
	my $tester = make_a_child_node( 'user', $test_db, 'catalog' );
	$tester->set_enumerated_attribute( 'user_type', 'DATA_EDITOR' );
	$tester->set_literal_attribute( 'name', 'joesmith' );
	$tester->set_literal_attribute( 'password', 'fdsKJ4' );

	# Define a utility app instance that testers will demonstrate with:
	my $test_setup_app = make_a_node( 'application_instance', $model );
	$test_setup_app->set_node_ref_attribute( 'blueprint', $setup_app );
	$test_setup_app->set_literal_attribute( 'name', 'test Setup' );
	# Describe the data link instance that the utility app will use to talk to the test database:
	my $test_setup_app_cl = make_a_child_node( 'catalog_link_instance', $test_setup_app, 'application' );
	$test_setup_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$test_setup_app_cl->set_node_ref_attribute( 'unrealized', $setup_app_cl );
	$test_setup_app_cl->set_node_ref_attribute( 'target', $test_db );
	$test_setup_app_cl->set_literal_attribute( 'local_dsn', 'test' );

	# Define a normal app instance that testers will demonstrate with:
	my $test_editor_app = make_a_node( 'application_instance', $model );
	$test_editor_app->set_node_ref_attribute( 'blueprint', $editor_app );
	$test_editor_app->set_literal_attribute( 'name', 'test People Watcher' );
	# Describe the data link instance that the normal app will use to talk to the test database:
	my $test_editor_app_cl = make_a_child_node( 'catalog_link_instance', $test_editor_app, 'application' );
	$test_editor_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$test_editor_app_cl->set_node_ref_attribute( 'unrealized', $editor_app_cl );
	$test_editor_app_cl->set_node_ref_attribute( 'target', $test_db );
	$test_editor_app_cl->set_literal_attribute( 'local_dsn', 'test' );

	##### NEXT SET 'DEMO' INSTANCE-TYPE DETAILS #####

	# Define the database catalog instance that marketers will demonstrate with:
	my $demo_db = make_a_node( 'catalog_instance', $model );
	$demo_db->set_node_ref_attribute( 'product', $dsp_oracle );
	$demo_db->set_node_ref_attribute( 'blueprint', $catalog_bp );
	$demo_db->set_literal_attribute( 'name', 'demo' );

	# Define the database user that owns the demo db schema:
	my $ownerI2 = make_a_child_node( 'user', $demo_db, 'catalog' );
	$ownerI2->set_enumerated_attribute( 'user_type', 'SCHEMA_OWNER' );
	$ownerI2->set_node_ref_attribute( 'match_owner', $owner );
	$ownerI2->set_literal_attribute( 'name', 'florence' );
	$ownerI2->set_literal_attribute( 'password', '0sfs8G' );

	# Define a 'normal' user that will work with the demo db:
	my $marketer = make_a_child_node( 'user', $demo_db, 'catalog' );
	$marketer->set_enumerated_attribute( 'user_type', 'DATA_EDITOR' );
	$marketer->set_literal_attribute( 'name', 'thainuff' );
	$marketer->set_literal_attribute( 'password', '9340sd' );

	# Define a utility app instance that marketers will demonstrate with:
	my $demo_setup_app = make_a_node( 'application_instance', $model );
	$demo_setup_app->set_node_ref_attribute( 'blueprint', $setup_app );
	$demo_setup_app->set_literal_attribute( 'name', 'demo Setup' );
	# Describe the data link instance that the utility app will use to talk to the demo database:
	my $demo_setup_app_cl = make_a_child_node( 'catalog_link_instance', $demo_setup_app, 'application' );
	$demo_setup_app_cl->set_node_ref_attribute( 'product', $dlp_odbc );
	$demo_setup_app_cl->set_node_ref_attribute( 'unrealized', $setup_app_cl );
	$demo_setup_app_cl->set_node_ref_attribute( 'target', $demo_db );
	$demo_setup_app_cl->set_literal_attribute( 'local_dsn', 'demo' );

	# Define a normal app instance that marketers will demonstrate with:
	my $demo_editor_app = make_a_node( 'application_instance', $model );
	$demo_editor_app->set_node_ref_attribute( 'blueprint', $editor_app );
	$demo_editor_app->set_literal_attribute( 'name', 'demo People Watcher' );
	# Describe the data link instance that the normal app will use to talk to the demo database:
	my $demo_editor_app_cl = make_a_child_node( 'catalog_link_instance', $demo_editor_app, 'application' );
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
			<command id="1" application="1" name="install_app_schema" command_type="DB_CREATE" command_arg="1" />
			<command id="2" application="1" name="remove_app_schema" command_type="DB_DELETE" command_arg="1" />
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
'
	);
}

######################################################################

sub test_circular_ref_prevention {
	my (undef, $class) = @_;
	my $model = $class->new_container();

	my $catalog_bp = make_a_node( 'catalog', $model );
	my $owner = make_a_child_node( 'owner', $catalog_bp, 'catalog' );
	my $schema = make_a_child_node( 'schema', $catalog_bp, 'catalog' );
	$schema->set_literal_attribute( 'name', 'gene' );
	$schema->set_node_ref_attribute( 'owner', $owner );

	my $vw1 = make_a_child_node( 'view', $schema, 'schema' );
	$vw1->set_enumerated_attribute( 'view_type', 'COMPOUND' );
	$vw1->set_literal_attribute( 'name', 'foo' );
	$vw1->set_enumerated_attribute( 'compound_op', 'UNION' );

	my $vw2 = make_a_child_node( 'view', $vw1, 'p_view' );
	$vw2->set_enumerated_attribute( 'view_type', 'SINGLE' );
	$vw2->set_literal_attribute( 'name', 'bar' );

	my $vw3 = make_a_child_node( 'view', $vw2, 'p_view' );
	$vw3->set_enumerated_attribute( 'view_type', 'SINGLE' );
	$vw3->set_literal_attribute( 'name', 'bz' );

	my $test1_passed = 0;
	my $test2_passed = 0;
	eval {
		$vw2->set_node_ref_attribute( 'p_view', $vw3 );
	};
	if( my $exception = $@ ) {
		if( UNIVERSAL::isa( $exception, 'Locale::KeyedText::Message' ) ) {
			if( $exception->get_message_key() eq 'SSM_N_SET_NREF_AT_CIRC_REF' ) {
				$test1_passed = 1;
			}
		}
	}
	eval {
		$vw3->clear_parent_node_attribute_name();
		$vw2->set_node_ref_attribute( 'p_view', $vw3 );
		$vw3->set_parent_node_attribute_name( 'p_view' );
	};
	if( my $exception = $@ ) {
		if( UNIVERSAL::isa( $exception, 'Locale::KeyedText::Message' ) ) {
			if( $exception->get_message_key() eq 'SSM_N_SET_P_NODE_ATNM_CIRC_REF' ) {
				$test2_passed = 1;
			}
		}
	}

	$model->with_all_nodes_test_mandatory_attributes();
	$model->destroy();

	return( $test1_passed, $test2_passed );
}

######################################################################

1;
