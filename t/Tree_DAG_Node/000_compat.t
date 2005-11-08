use strict;
use warnings;

use Test::More tests => 4;

use_ok( 'Tree::Compat', 'Tree::DAG_Node' );

my $tree = Tree::DAG_Node->new;
isa_ok( $tree, 'Tree::DAG_Node' );

my $real_tree = $tree->REAL_TREE;
isnt( $real_tree, $tree );
is( $real_tree, ${$tree} );
