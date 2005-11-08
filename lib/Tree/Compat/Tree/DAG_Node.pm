package Tree::Compat::Tree::DAG_Node;

use strict;
use warnings;

our $VERSION = '1.00';

package Tree::DAG_Node;

# Set %INC so that require() thinks Tree::DAG_Node has already been loaded
$INC{'Tree/DAG_Node.pm'} = $INC{'Tree::Compat::Tree::DAG_Node'};

use strict;
use warnings;

use Carp ();
use Scalar::Util qw( blessed weaken );
use Tree;

our $Debug = 0;

sub new {
    my $class = shift;
    $class = blessed($class) if blessed($class);

    my $tree = Tree->new();
    $tree->error_handler( $tree->DIE );

    my $self = bless \$tree, $class;

    $tree->meta->{compat}{object} = $self;
    weaken( $self );

    print "Constructing $self in class $class\n" if $Debug;

    $self->_init();

    return $self;
}

sub REAL_TREE { ${+shift} }

sub _init {
    my $self = shift;
    my $o = ref($_[0]) eq 'HASH' ? $_[0] : {};

    $self->$_($o) for map {
        "_init_${_}"
    } qw( mother daughters name attributes );

    return;
}

sub _init_mother {
    my $self = shift;
    my ($o) = @_;

    if ( exists $o->{mother} && blessed $o->{mother} ) {
        $o->{mother}->add_daughter( $self );
    }
}

sub _init_daughters {
    my $self = shift;
    my ($o) = @_;

    if ( exists $o->{daughters}
        && ref($o->{daughters}) eq 'ARRAY'
        && @{$o->{daughters}}
    ) {
        $self->set_daughters( @{$o->{daughters}} );
    }
}

sub _init_name {
    my $self = shift;
    my ($o) = @_;

    if ( exists $o->{name} ) {
        $self->name( $o->{name} );
    }
}

sub _init_attributes {
    my $self = shift;
    my ($o) = @_;

    if ( exists $o->{attributes} ) {
        $self->attributes( $o->{attributes} );
    }
}

sub daughters {
    my $tree = ${+shift};
    @_ && Carp::croak( "Don't set daughters with doughters anymore\n" );

    my @children = map { $_->meta->{compat}{object} } $tree->children;
    return wantarray ? @children : \@children;
}

sub mother {
    my $tree = ${+shift};
    @_ && Carp::croak( "Don't set daughters with doughters anymore\n" );

    if ( my $parent = $tree->parent ) {
        return $parent->meta->{compat}{object};
    }
    return;
}

sub add_daughter {
    my $tree = ${+shift};
    return unless @_;
    $tree->add_child(map { ${$_} } @_);
    return;
}
*add_daughters = \&add_daughter;

sub add_daughter_left {
    my $tree = ${+shift};
    return unless @_;
    $tree->add_child({ at => 0 }, map { ${$_} } @_);
    return;
}
*add_daughters_left = \&add_daughter_left;

sub new_daughter {
    my $self = shift;

    my $child = $self->new( @_ );
    $self->add_daughter( $child );
    return $child;
}

sub new_daughter_left {
    my $self = shift;

    my $child = $self->new( @_ );
    $self->add_daughter_left( $child );
    return $child;
}

sub remove_daughter {
    my $self = shift;
    Carp::croak "mother must be an object!" unless ref $self;

    return unless @_;

    ${$self}->remove_child( map { ${$_} } @_ );

    return;
}
*remove_daughters = \&remove_daughter;

sub unlink_from_mother {
    my $self = shift;
    if ( my $parent = $self->mother ) {
        $parent->remove_daughter( $self );
        return $parent;
    }
    return;
}

sub clear_daughters {
    my $self = shift;
    return unless @_;

    return map {
        $_->meta->{compat}{object}
    } ${$self}->remove_child( map { ${$_} } @_ );
}

sub set_daughters {
    my $self = shift;
    $self->clear_daughters;
    $self->add_daughters( @_ ) if @_;
}

sub replace_with {
    my $self = shift;
    my @replacements = @_;

    if ( !$self->mother ) {
        foreach my $node ( @replacements ) {
            if ( my $parent = $node->mother ) {
                $parent->remove_daughter( $node );
            }
        }
    }
    else {
        my $parent = $self->mother;
        @replacements = grep {
            my $_parent = $_->mother;
            $_ eq $self || !$_parent || $_parent ne $parent
        } @replacements;

        $parent->set_daughters(
            map {
                $_ eq $self ? (@replacements) : $_
            } $parent->daughters
        );
    }

    return( $self, @replacements );
}

sub replace_with_daughters {
    my $self = shift;
    $self->replace_with( $self->clear_daughters );
}

sub add_left_sister {
    my $self = shift;
    return unless @_;
    my @sisters = $self->replace_with( @_, $self );
    shift @sisters; pop @sisters; # Remove copies of $self.
    return @sisters;
}
*add_left_sisters = \&add_left_sister;

sub add_right_sister {
    my $self = shift;
    return unless @_;
    my @sisters = $self->replace_with( $self, @_ );
    shift @sisters; shift @sisters; # Remove copies of $self.
    return @sisters;
}
*add_right_sisters = \&add_left_sister;

sub name {
    my $tree = ${+shift};
    $tree->set_value( @_ ) if @_;
    return $tree->value;
}

sub attribute {
    my $tree = ${+shift};
    if ( @_ ) {
        Carp::carp "my parameter must be a reference" unless ref($_[0]);
        $tree->meta->{compat}{attributes} = $_[0];
    }
    return $tree->meta->{compat}{attributes};
}
*attributes = \&attribute;

sub is_node { return 1; }

sub ancestors {
    my $self = shift;

    my @ancestors = ( $self->mother )
        or return;

    while ( my $parent = $ancestors[-1]->mother ) {
        push @ancestors, $parent;
    }

    return @ancestors;
}

sub root {
    ${+shift}->root->meta->{compat}{object};
}

sub is_daughter_of {
    return $_[0]->parent eq $_[1];
}

sub self_and_descendants {
    return map { $_->meta->{compat}{object} } ${$_[0]}->traverse;
}

sub descendants {
    my $self = shift;
    my @list = $self->self_and_descendants;
    shift @list; # lose myself
    return @list;
}

sub leaves_under {
    my $tree = ${+shift};

    my @list;

    my $traversal = $tree->traverse;
    while ( my $node = $traversal->() ) {
        push @list, $node->meta->{compat}{object}
            if $node->is_leaf;
    }

    return @list;
}

sub depth_under {
    my $tree = ${+shift};

    my $max_depth = my $depth = $tree->depth;
    my $traversal = $tree->traverse;
    while ( my $node = $traversal->() ) {
        $max_depth = $node->depth if $node->depth > $max_depth;
    }

    return $max_depth - $depth;
}

sub generation {
}

sub generation_under {
}

sub self_and_sisters {
    my $self = shift;
    my $parent = $self->mother or return $self;
    return $parent->daughters;
}

sub sisters {
    my $self = shift;
    return grep { $_ ne $self } $self->self_and_sisters;
}

sub left_sisters {
    my $self = shift;
    my $parent = $self->mother or return;

    my @sisters;
    foreach my $sister ($parent->daughters) {
        last if $sister eq $self;
        push @sisters, $sister;
    }

    return @sisters;
}

sub left_sister {
    my $self = shift;
    my @sisters = $self->left_sisters
        or return;
    return $sisters[-1];
}

sub right_sisters {
    my $self = shift;
    my $parent = $self->mother or return;

    my @sisters;
    foreach my $sister (reverse $parent->daughters) {
        last if $sister eq $self;
        push @sisters, $sister;
    }

    return reverse @sisters;
}

sub right_sister {
    my $self = shift;
    my @sisters = $self->right_sisters
        or return;
    return $sisters[0];
}

sub my_daughter_index {
    my $self = shift;
    my $parent = $self->mother
        or return 0;

    return ${$parent}->get_index_for( ${$self} );
}

sub address {
    my $self = shift;
    my ($address) = @_;

    if ( defined $address && length $address ) {
        my @parts = map {$_ + 0}
                        $address =~ m/(\d+)/g; # generous!
        Carp::croak "Address \"$address\" is an ill-formed address" unless @parts;
        Carp::croak "Address \"$address\" must start with '0'" unless shift(@parts) == 0;

        my $current_node = $self->root;
        while ( @parts ) {
            my $index = shift @parts;
            my @daughters = $current_node->daughters;

            if ( $#daughters < $index ) {
                return;
            }
            $current_node = $daughters[$index];
        }

        return $current_node;
    }
    else {
        my @parts;
        my $current_node = $self;
        while ( my $parent = $current_node->mother ) {
            unshift @parts, $current_node->my_daughter_index;
            $current_node = $parent;
        }
        return join( ':', 0, @parts );
    }
}

sub common {
    my ($first, @others) = @_;
    return $first unless @others;

    foreach my $node (@others) {
        my %first_lineage;
        @first_lineage{$first, $first->ancestors} = undef;
        my $higher = undef; # the common of $first and $node
        my @my_lineage = $node->ancestors;

        Find_Common:
        while (@my_lineage) {
            if ( exists $first_lineage{$my_lineage[0]} ) {
                $higher = $my_lineage[0];
                last Find_Common;
            }
            shift @my_lineage;
        }
        return undef unless $higher;
        $first = $higher;
    }

    return $first;
}

sub common_ancestor {
    my ($first, @others) = @_;
    return $first->parent unless @others;

    my %ones;
    @ones{ @_ } = undef;

    my $common = $first->common( @others );
    if ( exists( $ones{$common} ) ) {
        return $common->mother;
    } else {
        return $common;
    }
}

sub walk_down {
    my ($this,$o) = @_;

    Carp::croak "I need options!" unless ref($o);
    Carp::croak "I need a callback or a callbackback" unless
        ( ref($o->{'callback'}) || ref($o->{'callbackback'}) );

    my $callback = ref($o->{'callback'}) ? $o->{'callback'} : undef;
    my $callbackback = ref($o->{'callbackback'}) ? $o->{'callbackback'} : undef;
    my $callback_status = 1;

    $callback_status = &{ $callback }( $this, $o ) if $callback;

    if($callback_status) {
        my @daughters = UNIVERSAL::can($this, 'is_node') ? @{$this->daughters} : ();
        if(@daughters) {
            $o->{'_depth'} += 1;
            foreach my $one (@daughters) {
                $one->walk_down($o) if UNIVERSAL::can($one, 'is_node');
            }
            $o->{'_depth'} -= 1;
        }
    }

    if($callbackback){
        if(UNIVERSAL::can($this, 'is_node')) {
            scalar( &{ $callbackback }( $this, $o ) );
        }
    }

    return;
}

sub dump_names {
    my($it, $o) = @_[0,1];
    $o = {} unless ref $o;
    $o->{'_depth'} ||= 0;
    $o->{'indent'} ||= '  ';
    $o->{'tick'} ||= '';

    my @out = ();
    $o->{'callback'} = sub {
        my($this, $o) = @_[0,1];
        push(@out,
            join('',
                $o->{'indent'} x $o->{'_depth'},
                $o->{'tick'},
                &Tree::DAG_Node::_dump_quote($this->name || $this),
                "\n"
            )
        );      
        return 1;
    };

    $it->walk_down($o);
    return @out;
}

sub random_network {
    Carp::croak( "random_network() unimplemented." );
}

sub lol_to_tree {
    my($class, $lol, $seen_r) = @_;
    $seen_r = {} unless ref($seen_r) eq 'HASH';
    return if ref($lol) && $seen_r->{$lol}++; # catch circularity

    $class = ref($class) || $class;
    my $node = $class->new();

    unless(ref($lol) eq 'ARRAY') {  # It's a terminal node.
        $node->name($lol) if defined $lol;
        return $node;
    }
    return $node unless @$lol;  # It's a terminal node, oddly represented

    #  It's a non-terminal node.

    my @options = @$lol; 
    unless(ref($options[-1]) eq 'ARRAY') {
        # This is what separates this method from simple_lol_to_tree
        $node->name(pop(@options));
    }

    foreach my $d (@options) {  # Scan daughters (whether scalars or listrefs)
        $node->add_daughter( $class->lol_to_tree($d, $seen_r) );  # recurse!
    }

    return $node;
}

sub tree_to_lol_notation {
    my($it, $o) = @_;
    my $root = $it;

    $o = {} unless ref $o;
    my @out = ();
    $o->{'_depth'} ||= 0;
    $o->{'multiline'} = 0 unless exists($o->{'multiline'});

    my $line_end;
    if($o->{'multiline'}) {
        $o->{'indent'} ||= '  ';
        $line_end = "\n";
    } else {
        $o->{'indent'} ||= '';
        $line_end = '';
    }

    $o->{'callback'} = sub {
        my($this, $o) = @_[0,1];
        push(@out,
            $o->{'indent'} x $o->{'_depth'},
            "[$line_end",
        );      
        return 1;
    };

    $o->{'callbackback'} = sub {
        my($this, $o) = @_[0,1];
        my $name = $this->name;
        if(!defined($name)) {
            $name = 'undef';
        } else {
            $name = &Tree::DAG_Node::_dump_quote($name);
        }
        push(@out,
                $o->{'indent'} x ($o->{'_depth'} + 1),
                "$name$line_end",
                $o->{'indent'} x $o->{'_depth'},
                "], $line_end",
        );
        return 1;
    };

    $it->walk_down($o);
    return join('', @out);
}

sub tree_to_lol {
    # I haven't /rigorously/ tested this.
    my($it, $o) = @_;
    $o = {} unless ref $o;

    my $out = [];
    my @lol_stack = ($out);
    $o->{'callback'} = sub {
        my($this, $o) = @_[0,1];
        my $new = [];
        push @{$lol_stack[-1]}, $new;
        push(@lol_stack, $new);
        return 1;
    };

    $o->{'callbackback'} = sub {
        my($this, $o) = @_[0,1];
        push @{$lol_stack[-1]}, $this->name;
        pop @lol_stack;
        return 1;
    };

    $it->walk_down($o);
    die "totally bizarre error 12416" unless ref($out->[0]);
    $out = $out->[0]; # the real root
    return $out;
}

sub simple_lol_to_tree {
    my($class, $lol, $seen_r) = @_;
    $class = ref($class) || $class;
    $seen_r = {} unless ref($seen_r) eq 'HASH';
    return if ref($lol) && $seen_r->{$lol}++; # catch circularity

    my $node = $class->new();

    unless(ref($lol) eq 'ARRAY') {  # It's a terminal node.
        $node->name($lol) if defined $lol;
        return $node;
    }

    #  It's a non-terminal node.
    foreach my $d (@$lol) { # scan daughters (whether scalars or listrefs)
        $node->add_daughter( $class->simple_lol_to_tree($d, $seen_r) );  # recurse!
    }

    return $node;
}

sub tree_to_simple_lol {
    # I haven't /rigorously/ tested this.
    my $root = $_[0];

    return $root->name unless scalar($root->daughters);
    # special case we have to nip in the bud

    my($it, $o) = @_[0,1]; # $o is currently unused anyway
    $o = {} unless ref $o;

    my $out = [];
    my @lol_stack = ($out);
    $o->{'callback'} = sub {
        my($this, $o) = @_[0,1];
        my $new;
        $new = scalar($this->daughters) ? [] : $this->name;
            # Terminal nodes are scalars, the rest are listrefs we'll fill in
            # as we recurse the tree below here.
        push @{$lol_stack[-1]}, $new;
        push(@lol_stack, $new);
        return 1;
    };

    $o->{'callbackback'} = sub { pop @lol_stack; return 1; };
    $it->walk_down($o);
    die "totally bizarre error 12416" unless ref($out->[0]);
    $out = $out->[0]; # the real root
    return $out;
}

sub tree_to_simple_lol_notation {
    my($it, $o) = @_[0,1];
    $o = {} unless ref $o;
    my @out = ();
    $o->{'_depth'} ||= 0;
    $o->{'multiline'} = 0 unless exists($o->{'multiline'});

    my $line_end;
    if($o->{'multiline'}) {
        $o->{'indent'} ||= '  ';
        $line_end = "\n";
    } else {
        $o->{'indent'} ||= '';
        $line_end = '';
    }

    $o->{'callback'} = sub {
        my($this, $o) = @_[0,1];
        if(scalar($this->daughters)) {   # Nonterminal
            push(@out,
                $o->{'indent'} x $o->{'_depth'},
                "[$line_end",
            );
        } else {   # Terminal
            my $name = $this->name;
            push @out,
            $o->{'indent'} x $o->{'_depth'},
            defined($name) ? &Tree::DAG_Node::_dump_quote($name) : 'undef',
            ",$line_end";
        }
        return 1;
    };

    $o->{'callbackback'} = sub {
        my($this, $o) = @_[0,1];
        push(@out,
                $o->{'indent'} x $o->{'_depth'},
                "], $line_end",
        ) if scalar($this->daughters);
        return 1;
    };

    $it->walk_down($o);
    return join('', @out);
}

sub draw_ascii_tree {
    # Make a "box" for this node and its possible daughters, recursively.

    # The guts of this routine are horrific AND recursive!

    # Feel free to send me better code.  I worked on this until it
    #  gave me a headache and it worked passably, and then I stopped.

    my $it = $_[0];
    my $o = ref($_[1]) ? $_[1] : {};
    my(@box, @daughter_boxes, $width, @daughters);
    @daughters = $it->daughters;

    # $it->no_cyclicity;

    $o->{'no_name'}   = 0 unless exists $o->{'no_name'};
    $o->{'h_spacing'} = 1 unless exists $o->{'h_spacing'};
    $o->{'h_compact'} = 1 unless exists $o->{'h_compact'};
    $o->{'v_compact'} = 1 unless exists $o->{'v_compact'};

    my $printable_name;
    if($o->{'no_name'}) {
        $printable_name = '*';
    } else {
        $printable_name = $it->name || $it;
        $printable_name =~ tr<\cm\cj\t >< >s;
        $printable_name = "<$printable_name>";
    }

    if(!scalar(@daughters)) { # I am a leaf!
        # Now add the top parts, and return.
        @box = ("|", $printable_name);
    } else {
        @daughter_boxes = map { &draw_ascii_tree($_, $o) } @daughters;

        my $max_height = 0;
        foreach my $box (@daughter_boxes) {
        my $h = @$box;
        $max_height = $h if $h > $max_height;
        }

        @box = ('') x $max_height; # establish the list

        foreach my $one (@daughter_boxes) {
        my $length = length($one->[0]);
        my $height = @$one;

        #now make all the same height.
        my $deficit = $max_height - $height;
        if($deficit > 0) {
            push @$one, ( scalar( ' ' x $length ) ) x $deficit;
            $height = scalar(@$one);
        }


        # Now tack 'em onto @box
        ##########################################################
        # This used to be a sub of its own.  Ho-hum.

        my($b1, $b2) = (\@box, $one);
        my($h1, $h2) = (scalar(@$b1), scalar(@$b2));

        my(@diffs, $to_chop);
        if($o->{'h_compact'}) { # Try for h-scrunching.
            my @diffs;
            my $min_diff = length($b1->[0]); # just for starters
            foreach my $line (0 .. ($h1 - 1)) {
            my $size_l = 0; # length of terminal whitespace
            my $size_r = 0; # length of initial whitespace
            $size_l = length($1) if $b1->[$line] =~ /( +)$/s;
            $size_r = length($1) if $b2->[$line] =~ /^( +)/s;
            my $sum = $size_l + $size_r;
        
            $min_diff = $sum if $sum < $min_diff;
            push @diffs, [$sum, $size_l, $size_r];
            }
            $to_chop = $min_diff - $o->{'h_spacing'};
            $to_chop = 0 if $to_chop < 0;
        }

        if(not(  $o->{'h_compact'} and $to_chop  )) {
            # No H-scrunching needed/possible
            foreach my $line (0 .. ($h1 - 1)) {
            $b1->[ $line ] .= $b2->[ $line ] . (' ' x $o->{'h_spacing'});
            }
        } else {
            # H-scrunching is called for.
            foreach my $line (0 .. ($h1 - 1)) {
            my $r = $b2->[$line]; # will be the new line
            my $remaining = $to_chop;
            if($remaining) {
                my($l_chop, $r_chop) = @{$diffs[$line]}[1,2];
        
                if($l_chop) {
                if($l_chop > $remaining) {
                    $l_chop = $remaining;
                    $remaining = 0;
                } elsif($l_chop == $remaining) {
                    $remaining = 0;
                } else { # remaining > l_chop
                    $remaining -= $l_chop;
                }
                }
                if($r_chop) {
                if($r_chop > $remaining) { 
                    $r_chop = $remaining;
                    $remaining = 0;
                } elsif($r_chop == $remaining) {
                    $remaining = 0;
                } else { # remaining > r_chop
                    $remaining -= $r_chop; # should never happen!
                }
                }

                substr($b1->[$line], -$l_chop) = '' if $l_chop;
                substr($r, 0, $r_chop) = '' if $r_chop;
            } # else no-op
            $b1->[ $line ] .= $r . (' ' x $o->{'h_spacing'});
            }
            # End of H-scrunching ickyness
        }
        # End of ye big tack-on

        }
        # End of the foreach daughter_box loop

        # remove any fencepost h_spacing
        if($o->{'h_spacing'}) {
        foreach my $line (@box) {
            substr($line, -$o->{'h_spacing'}) = '' if length($line);
        }
        }

        # end of catenation
        die "SPORK ERROR 958203: Freak!!!!!" unless @box;

        # Now tweak the pipes
        my $new_pipes = $box[0];
        my $pipe_count = $new_pipes =~ tr<|><+>;
        if($pipe_count < 2) {
        $new_pipes = "|";
        } else {
        my($init_space, $end_space);

        # Thanks to Gilles Lamiral for pointing out the need to set to '',
        #  to avoid -w warnings about undeffiness.

        if( $new_pipes =~ s<^( +)><>s ) {
            $init_space = $1;
        } else {
            $init_space = '';
        }

        if( $new_pipes =~ s<( +)$><>s ) {
            $end_space  = $1
        } else {
            $end_space = '';
        }

        $new_pipes =~ tr< ><->;
        substr($new_pipes,0,1) = "/";
        substr($new_pipes,-1,1) = "\\";

        $new_pipes = $init_space . $new_pipes . $end_space;
        # substr($new_pipes, int((length($new_pipes)), 1)) / 2) = "^"; # feh
        }

        # Now tack on the formatting for this node.
        if($o->{'v_compact'} == 2) {
        if(@daughters == 1) {
            unshift @box, "|", $printable_name;
        } else {
            unshift @box, "|", $printable_name, $new_pipes;
        }
        } elsif ($o->{'v_compact'} == 1 and @daughters == 1) {
        unshift @box, "|", $printable_name;
        } else { # general case
        unshift @box, "|", $printable_name, $new_pipes;
        }
    }

    # Flush the edges:
    my $max_width = 0;
    foreach my $line (@box) {
        my $w = length($line);
        $max_width = $w if $w > $max_width;
    }
    foreach my $one (@box) {
        my $space_to_add = $max_width - length($one);
        next unless $space_to_add;
        my $add_left = int($space_to_add / 2);
        my $add_right = $space_to_add - $add_left;
        $one = (' ' x $add_left) . $one . (' ' x $add_right);
    }

    return \@box; # must not return a null list!
}

sub copy_at_and_under {
    my($from, $o) = @_[0,1];
    $o = {} unless ref $o;
    my @daughters = map($_->copy_at_and_under($o), $from->daughters);
    my $to = $from->copy($o);
    $to->set_daughters(@daughters) if @daughters;
    return $to;
}

sub copy_tree {
    my($this, $o) = @_[0,1];
    my $root = $this->root;
    $o = {} unless ref $o;
    
    my $new_root = $root->copy_at_and_under($o);
    
    return $new_root;
}

sub copy {
    my $self = shift;
    my ($o) = @_;
    $o = {} unless $o;

    my $class = blessed($self) ? blessed($self) : $self;

    my $tree = Tree->new();
    $tree->error_handler( $tree->DIE );

    my $clone = bless \$tree, $class;

    $tree->meta->{compat}{object} = $clone;
    weaken( $clone );

    $clone->_init_mother;
    $clone->_init_daughters;

    if($o->{'no_attribute_copy'}) {
        $clone->attributes( $self->attributes );
    }
    else {
        if(my $attrib_copy = ref $self->attributes) {
            if($attrib_copy eq 'HASH') {
                $clone->attributes( { %{$self->attributes} } );
            } elsif ($attrib_copy = UNIVERSAL::can($self->attributes, 'copy') ) {
                $clone->attributes( &{$attrib_copy}($self) );
            }
        }
    }

    $o->{'from_to'}{$self} = $clone;
    return $clone;
}

sub delete_tree {
    my $it = $_[0];
    $it->root->walk_down({ # has to be callbackback, not callback
        'callbackback' => sub {
            ${$_[0]}->parent->remove_child( ${$_[0]} );
            bless($_[0], 'DEADNODE');
            return 1;
        }
    });
    return;
}

sub DEADNODE::delete_tree { return; }

###########################################################################
# stolen from MIDI.pm

sub _dump_quote {
  my @stuff = @_;
  return
    join(", ",
    map
     { # the cleaner-upper function
       if(!length($_)) { # empty string
         "''";
       } elsif( m/^-?\d+(?:\.\d+)?$/s ) { # a number
         $_;
       } elsif( # text with junk in it
          s<([^\x20\x21\x23\x27-\x3F\x41-\x5B\x5D-\x7E])>
           <'\\x'.(unpack("H2",$1))>eg
         ) {
         "\"$_\"";
       } else { # text with no junk in it
         s<'><\\'>g;
         "\'$_\'";
       }
     }
     @stuff
    );
}

###########################################################################

1;
__END__
