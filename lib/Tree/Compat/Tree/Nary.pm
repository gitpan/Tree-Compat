package Tree::Compat::Tree::Nary;

use strict;
use warnings;

our $VERSION = '1.00';

package Tree::Nary;

# Some notes:
# 1) Tree::Nary has all class methods that take an object as an argument.
#    Hence, the "sub foo { shift;" idiom.
# 2) The children look to be implemented as a linked list, not an array.
#    This may cause problems in passing the tests.

# Set %INC so that require() thinks Tree::Nary has already been loaded
$INC{'Tree/Nary.pm'} = $INC{'Tree::Compat::Tree::Nary'};

use strict;
use warnings;

use Scalar::Util qw( blessed weaken );
use Tree;
use Tree::Binary; # For the in-order traversal constant

use vars qw($TRUE $FALSE);
use vars qw($TRAVERSE_LEAFS $TRAVERSE_NON_LEAFS $TRAVERSE_ALL $TRAVERSE_MASK);
use vars qw($IN_ORDER $PRE_ORDER $POST_ORDER $LEVEL_ORDER);

# Booleans
*TRUE  = \1;
*FALSE = \0;

# Tree traverse flags
*TRAVERSE_LEAFS		= \(1 << 0);					# Only leaf nodes should be visited.
*TRAVERSE_NON_LEAFS	= \(1 << 1);					# Only non-leaf nodes should be visited.
*TRAVERSE_ALL		= \($TRAVERSE_LEAFS | $TRAVERSE_NON_LEAFS);	# All nodes should be visited.
*TRAVERSE_MASK		= \0x03;

# Tree traverse orders
*IN_ORDER		= \1;
*PRE_ORDER		= \2;
*POST_ORDER		= \3;
*LEVEL_ORDER		= \4;

sub new {
    my $class = shift;
    my ($data) = @_;

    my $tree = Tree->new();
    $tree->error_handler( $tree->DIE );

    my $self = bless \$tree, $class;

    $tree->meta->{compat}{object} = $self;
    weaken( $self );

    if ( defined $data ) {
        $tree->set_value( $data );
    }

    return $self;
}

sub REAL_TREE { ${+shift} }

sub unlink {
}

sub is_root { shift;
    # Check against next and prev too?
    ${$_[0]}->is_root;
}

sub is_leaf { shift;
    ${$_[0]}->is_leaf;
}

sub _parent {
    my $parent = ${$_[0]}->parent;
    return $parent->meta->{compat}{object} if $parent;
    return;
}

sub is_ancestor { shift;
    my ($self, $child) = @_;

    return unless $self && $child;

    while ( $child ) {
        my $parent = $child->_parent;

        if ( $parent && $parent == $self ) {
            return 1;
        }

        $child = $parent;
    }

    return;
}

sub get_root { shift;
    ${$_[0]}->root->meta->{compat}{object};
}

sub depth { shift;
    ${$_[0]}->depth;
}

sub reverse_children { shift;
    my $self = shift;
    my $tree = ${$self};

    $tree->add_child( reverse $tree->remove_child( $tree->children ) );

    return;
}

sub max_height { shift;
    ${$_[0]}->height;
}

sub n_children { shift;
    scalar ${$_[0]}->children;
}

sub child_position { shift;
    my ($self, $child) = @_;

    if ( !$self || !$child || $self ne $child->_parent ) {
        return -1;
    }

    ${$self}->get_index_for( ${$child} );
}

sub child_index { shift;
    my ($self, $data) = @_;

    return -1 unless defined $self;

    my @children = ${$self}->children;
    foreach my $n ( 0 .. $#children ) {
        if ( $children[$n]->value eq $data ) {
            return $n;
        }
    }

    return;
}

sub first_sibling { shift;
    my ($self) = @_;
    return unless $self;

    my $parent = $self->_parent
        or return $self;
    return ${$parent}->children( 0 )->meta->{compat}{object};
}

sub next_sibling { shift;
    my ($self) = @_;
    return unless $self;

    my $tree = ${$self};
    my $i = $tree->parent->get_index_for( $tree ) + 1;
    my $num_children = $tree->parent->children;
    return unless $i <= $num_children;
    return ${$self}->children( $i )->meta->{compat}{object};
}

sub prev_sibling { shift;
    my ($self) = @_;
    return unless $self;

    my $tree = ${$self};
    my $i = $tree->parent->get_index_for( $tree ) - 1;
    return unless $i >= 0;
    return ${$self}->children( $i )->meta->{compat}{object};
}

sub last_sibling { shift;
    my ($self) = @_;
    return unless $self;

    my $parent = $self->_parent
        or return $self;
    return ${$parent}->children(
        scalar ${$parent}->children
    )->meta->{compat}{object};
}

sub n_nodes { shift;
    my ($self, $flags) = @_;
    return 0 unless $self;
    return 0 unless $flags <= $TRAVERSE_MASK;

    ${$self}->size;
}

sub first_child { shift;
    return unless $_[0];
    ${$_[0]}->children( 0 )->meta->{compat}{object};
}

sub last_child { shift;
    return unless $_[0];
    return ${$_[0]}->children( scalar ${$_[0]}->children );
}

sub nth_child { shift;
    return unless $_[0];
    return ${$_[0]}->children( $_[1] );
}

sub insert { shift;
    my ($self, $position, $child) = @_;

    if ( !$self && !defined $child && !$child->is_root ) {
        return $child;
    }

    ${$self}->add_child( { at => $position }, ${$child} );
    return $child;
}

sub insert_data {
    my ($class, $parent, $sibling, $data ) = @_;
    $class->insert( $parent, $sibling, $class->new( $data ) );
}

sub insert_before {
    my $class = shift;
    my ($self, $sibling, $child) = @_;

    if ( !$self && !defined $child && !$child->is_root ) {
        return $child;
    }

    if ( defined $sibling ) {
        if ( $sibling->_parent ne $self ) {
            return $child;
        }
        my $i = $class->child_position( $self, $sibling );
        ${$self}->add_child( { at => $i }, $child );
    }
    else {
        ${$self}->add_child( ${$child} );
    }

    return $child;
}

sub insert_data_before {
    my ($class, $parent, $sibling, $data ) = @_;
    $class->insert_before( $parent, $sibling, $class->new( $data ) );
}

sub append() {
	my ($self, $parent, $node) = @_;

	$self->insert_before($parent, undef, $node);
}

sub append_data() {
	my ($self, $parent, $data) = @_;

	$self->insert_before($parent, undef, $self->new($data));
}

sub prepend() {
	my ($self, $parent, $node) = @_;

    return $node unless $parent;

    ${$parent}->add_child( { at => 0 }, $node );
}

sub prepend_data() {
	my ($self, $parent, $data) = @_;

	$self->prepend($parent, $self->new($data));
}

sub traverse {
	my ($self, $root, $order, $flags, $depth, $funcref, $argref) = @_;

    unless (
        $root && $funcref && $order <= $LEVEL_ORDER
     && $flags < $TRAVERSE_MASK || ($depth == -1 || $depth > 0)
    ) {
		return;
	}

    my %convert = (
        $PRE_ORDER   => Tree->PRE_ORDER,
        $POST_ORDER  => Tree->POST_ORDER,
        $LEVEL_ORDER => Tree->LEVEL_ORDER,
        $IN_ORDER    => Tree::Binary->IN_ORDER,
    );

    my $traversal = ${$root}->traverse( Tree->PRE_ORDER );

    while ( my $node = $traversal->() ) {
        # $depth == 0 cannot happen
        # -1 will never be greater than $node->depth
        next if $depth > $node->depth;

        if ( $node->is_leaf ) {
            if ($flags & $TRAVERSE_LEAFS) {
                if ( $funcref->($node->meta->{compat}{object}, $argref) ) {
                    last;
                }
            }
        }
        else {
            if ($flags & $TRAVERSE_NON_LEAFS) {
                if ( $funcref->($node->meta->{compat}{object}, $argref) ) {
                    last;
                }
            }
        }
    }

    return;
}

sub find {
    my ($self, $root, $order, $flags, $data) = @_;

	unless ( $root && $order <= $LEVEL_ORDER && $flags <= $TRAVERSE_MASK ) {
		return;
	}

    my $found;
    $self->traverse(
        $root, $order, $flags, -1, sub {
            my $node = shift;
            if ( $data eq ${$node}->value ) {
                $found = $node;
                return 1;
            }

            return;
        },
    );

    return $found;
}

sub find_child {
    my ($self, $node, $flags, $data) = @_;

	unless ( $node && $flags <= $TRAVERSE_MASK ) {
		return;
	}

    foreach my $node ( ${node}->children ) {
        if (
            $node->is_leaf && $flags & $TRAVERSE_LEAFS
         || $flags & $TRAVERSE_NON_LEAFS
        ) {
            return $node if $node->value eq $data;
        }
    }
}

sub children_foreach {
    my ($self, $node, $flags, $funcref, $argref) = @_;

	unless ( $node && $funcref && $flags <= $TRAVERSE_MASK ) {
		return;
	}

    foreach my $node ( ${node}->children ) {
        if (
            $node->is_leaf && $flags & $TRAVERSE_LEAFS
         || $flags & $TRAVERSE_NON_LEAFS
        ) {
            return $node if $funcref->( $node->meta->{compat}{object}, $argref);
        }
    }
}

sub tsort {
    my ($self, $node) = @_;

    return if $self->is_leaf( $node );

    my $tree = ${$node};
    my @children = sort {
        $b->value cmp $a->value
    } $tree->remove_child( $tree->children );

    $tree->add_child( @children );
    $self->tsort( $_->meta->{compat}{object} ) for @children;

    return;
}

sub normalize {
    my ($self, $node) = @_;

    return '*' if $self->is_leaf( $node );

    return '(' . join('',
        sort map { $self->normalize( $_->meta->{compat}{object} ) } ${$node}->children
    ) . ')';
}

sub is_identical {
    my ($self, $n1, $n2) = @_;

    my $tree1 = ${$n1};
    my $tree2 = ${$n2};

    return if $tree1->value ne $tree2->value;

    # If they have the same number of children, their leaf-ness has been
    # checked - a leaf will have 0 children.
    my @c1 = $tree1->children;
    my @c2 = $tree2->children;
    return if @c1 != @c2;

    for ( 0 .. $#c1 ) {
        return unless $self->is_identical(
            map { $_->meta->{compat}{object} } $c1[$_], $c2[$_]
        );
    }

    return 1;
}

sub has_same_struct {
    my ($self, $n1, $n2) = @_;

    return $self->normalize( $n1 ) eq $self->normalize( $n2 );
}

1;
__END__
