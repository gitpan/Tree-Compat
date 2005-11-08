use strict;
use warnings;

use Test::More tests => 4;

use_ok( 'Tree::Compat', 'Tree::Nary' );

my $tree = Tree::Nary->new;
isa_ok( $tree, 'Tree::Nary' );

my $real_tree = $tree->REAL_TREE;
isnt( $real_tree, $tree );
is( $real_tree, ${$tree} );
