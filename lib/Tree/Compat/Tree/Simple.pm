package Tree::Compat::Tree::Simple;

use strict;
use warnings;

our $VERSION = '1.00';

package Tree::Simple;

# Set %INC so that require() thinks Tree::Simple has already been loaded
$INC{'Tree/Simple.pm'} = $INC{'Tree::Compat::Tree::Simple'};

use strict;
use warnings;

use Scalar::Util qw( blessed weaken );
use Tree;

use constant ROOT => 'root';

sub new {
    my $class = shift;
    my ($value, $parent) = @_;

    my $tree = Tree->new( $value );
    $tree->error_handler( $tree->DIE );

    my $self = bless \$tree, $class;

    $tree->meta->{compat}{object} = $self;
    weaken( $self );

    $self->_init( $value, $parent, [] );

    return $self;
}

sub REAL_TREE { ${+shift} }

sub _init {
    my $self = shift;
    my $tree = ${$self};
    my ($value, $parent, $children) = @_;

    ($tree->meta->{compat}{uid}) = "$self" =~ /\((.*?)\)$/;

    if ( blessed $parent && $parent->isa( __PACKAGE__ ) ) {
        $parent->addChild( $self );
    }
    elsif ( defined $parent && $parent ne $self->ROOT ) {
        die "Insufficient Arguments : parent argument must be a Tree::Simple object";
    }

    # Untested code
    if ( @$children ) {
        $self->addChild( @$children );
    }

    return $self;
}

sub _setParent {}

sub isRoot { ${+shift}->is_root }
sub isLeaf { ${+shift}->is_leaf }

sub setNodeValue {
    my $tree = ${+shift};
    my ($value) = @_;
    (defined($value)) || die "Insufficient Arguments : must supply a value for node";
    $tree->set_value( $value );
}
sub getNodeValue { ${+shift}->value }

sub getIndex {
    my $tree = ${+shift};

    return -1 if $tree->is_root;
    my ($index) = $tree->parent->get_index_for( $tree );
    return $index;
}

sub getDepth { ${+shift}->depth - 1 }
sub fixDepth {}
sub getParent {
    my $self = shift;
    my $parent = ${$self}->parent;
    if ( $parent ) {
        return $parent->meta->{compat}{object};
    }
    else {
        return $self->ROOT;
    }
}

sub size { ${+shift}->size }
sub height { ${+shift}->height }

sub getChildCount { scalar ${+shift}->children }

sub addChild {
    my $tree = ${+shift};
	(@_) 
		|| die "Insufficient Arguments : no tree(s) to insert";	
    (blessed $_[0] && $_[0]->isa( __PACKAGE__ ))
        || die "Insufficient Arguments : Child must be a Tree::Simple object";
    return $tree->add_child(map { ${$_} } @_)
        ? $tree->meta->{compat}{object}
        : ();
}
*addChildren = \&addChild;

sub insertChild {
    my $self = shift;
    my $index = shift;
	(defined($index)) 
		|| die "Insufficient Arguments : Cannot insert child without index";
	# check the bounds of our children 
	# against the index given
    my $child_count = $self->getChildCount();
	($index <= $child_count)
		|| die "Index Out of Bounds : got ($index) expected no more than ($child_count).";
	(@_) 
		|| die "Insufficient Arguments : no tree(s) to insert";	

    foreach (@_) {
        next if blessed $_ && $_->isa( __PACKAGE__ );

        die "Insufficient Arguments : Child must be a Tree::Simple object";
    }

    my $tree = ${$self};
    return $tree->add_child({ at => $index }, map { ${$_} } @_ )
        ? $tree->meta->{compat}{object}
        : ();
}
*insertChildren = \&insertChild;

sub removeChild {
    my $self = shift;
    my ($child_to_remove) = @_;

    (defined($child_to_remove))
        || die "Insufficient Arguments : you must specify a child to remove";
    if (ref($child_to_remove)) {
        (blessed($child_to_remove) && $child_to_remove->isa(__PACKAGE__))
            || die "Insufficient Arguments : Only valid child type is a Tree::Simple object";
        my $found = 0;
        foreach my $child ( $self->getAllChildren() ) {
            "$child" eq "$child_to_remove" or next;
            $found = 1;
            last;
        }
        if ( !$found ) {
            die "Child Not Found : cannot find object ($child_to_remove) in self";
        }
    }

    my @return = map {
        $_->meta->{compat}{object}
    } ${$self}->remove_child( map {
        ref $_ ? ${$_} : $_
    } @_ );
    wantarray ? @return : $return[0];
}

sub removeChildAt {
    my $self = shift;

    my ($index) = @_;
    (defined $index)
        || die "Insufficient Arguments : Cannot remove child without index.";
    ((my $child_count = $self->getChildCount()) != 0) 
        || die "Illegal Operation : There are no children to remove";		
	($index < $child_count)
		|| die "Index Out of Bounds : got ($index) expected no more than ($child_count)";		
    $self->removeChild( @_ );
}

sub getAllChildren {
    my $tree = ${+shift};
    my @children = map { $_->meta->{compat}{object} } $tree->children( @_ );
    return wantarray ? @children : \@children;
}

sub getChild {
    my $self = shift;
    my ($index) = @_;
    (defined($index)) 
		|| die "Insufficient Arguments : Cannot get child without index";

    my @children = $self->getAllChildren( @_ );
    return $children[0];
}

sub addSibling {
    my $self = shift;
	(!$self->isRoot()) 
		|| die "Insufficient Arguments : cannot add a sibling to a ROOT tree";
    $self->getParent->addChild( @_ );
}
sub addSiblings {
    my $self = shift;
	(!$self->isRoot()) 
		|| die "Insufficient Arguments : cannot add siblings to a ROOT tree";
    $self->getParent->addChild( @_ );
}

sub insertSibling {
    my $self = shift;
	(!$self->isRoot()) 
		|| die "Insufficient Arguments : cannot insert sibling(s) to a ROOT tree";
    $self->getParent->insertChild( @_ );
}
*insertSiblings = \&insertSibling;

sub getSibling {
    my $self = shift;
	(!$self->isRoot()) 
		|| die "Insufficient Arguments : cannot get siblings from a ROOT tree";	
    $self->getParent->getChild( @_ );
}
sub getAllSiblings {
    my $self = shift;
	(!$self->isRoot()) 
		|| die "Insufficient Arguments : cannot get siblings from a ROOT tree";	
    return $self->getParent->getAllChildren;
}

sub traverse {
    my $tree = ${+shift};
    my ($func) = @_;

	(defined($func)) || die "Insufficient Arguments : Cannot traverse without traversal function";
	(ref($func) eq "CODE") || die "Incorrect Object Type : traversal function is not a function";

    my $traversal = $tree->traverse;

    # Tree::Simple's traverse doesn't include $self
    $traversal->();

    while ( my $node = $traversal->() ) {
        $func->( $node->meta->{compat}{object} );
    }
}

sub accept {
	my ($self, $visitor) = @_;
    # it must be a blessed reference and ...
	(blessed($visitor) && 
        # either a Tree::Simple::Visitor object, or ...
        ($visitor->isa("Tree::Simple::Visitor") || 
            # it must be an object which has a 'visit' method avaiable
            $visitor->can('visit')))
		|| die "Insufficient Arguments : You must supply a valid Visitor object";
	$visitor->visit($self);
}

sub clone {
    my $self = shift;
    my $clone = blessed($self)->new( _clone_value( $self->getNodeValue, {} ) );
    if ( my @children = $self->getAllChildren ) {
        $clone->addChild( map { $_->clone( $_->getNodeValue ) } @children );
    }
    return $clone;
}

sub _clone_value {
    my ($node, $seen) = @_;

    return $node unless ref $node;
    return $seen->{$node} if exists $seen->{$node};

    my $clone;
    if ( blessed( $node ) ) {
        if ( $node->can( 'clone' ) ) {
            $clone = $node->clone;
        }
        else {
            $clone = $node;
        }
    }
    else {
        if ( ref($node) eq 'SCALAR' or ref($node) eq 'REF' ) {
            $clone = \my $var;
            ${$clone} = _clone_value( ${$node}, $seen );
        }
        elsif( ref($node) eq 'ARRAY' ) {
            $clone = [
                map { _clone_value( $_, $seen ) } @{$node}
            ];
        }
        elsif( ref($node) eq 'HASH' ) {
            $clone = {};
            while ( my ($k, $v) = each %$node ) {
                $clone->{$k} = _clone_value( $v, $seen );
            }
        }
        else {
            $clone = $node;
        }
    }

    $seen->{$node} = $clone;
    return $clone;
}

sub cloneShallow {
    my $self = shift;
    die <<'END_DIE';
This method is unimplemented. You don't want to do this. The problem is that
$self->getChildAt(0)->getParent is not guaranteed to equal $self, and that is
a problem.
END_DIE
}

sub DESTROY {}

sub getUID { ${+shift}->meta->{compat}{uid} }
sub setUID {
    ($_[1]) || die "Insufficient Arguments : Custom Unique ID's must be a true value";
    ${+shift}->meta->{compat}{uid} = $_[1];
}

1;
__END__
