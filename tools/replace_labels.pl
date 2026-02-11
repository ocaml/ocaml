#!/usr/bin/env perl

#
# Hash which contains an entry "$valname:$label" to keep $label of $valname.
#
my %keep_labels;

#
# the last "val $name" that we have visited.
#
my $last_val_name;

sub handle_normal_comment {
    my $comment = shift(@_);

    if ($comment =~ /\A\(\*\s+#KEEP-LABELS\s*\[([^\]]*)\]\s*\*\)\Z/) {
        # This is a pragma comment
        my @argv = split ' ', $1;
	foreach my $arg (@argv) {
	    my @a = split(/:/, $arg);
	    my $name_of_val = shift(@a);
	    for my $label (@a) {
		$keep_labels{"${name_of_val}:${label}"} = 1;
	    }
	}
    } else {
         print $comment;
    }
}

sub handle_whitespace {
    my $ws = shift(@_);
    print $ws;
}

sub handle_doc_comment {
    my $comment = shift(@_);
    if ($last_val_name) {
        my $replacement = '$keep_labels{"$last_val_name:$1"} ? " ~$1" : " $1"';

        # /ee treats $replacement as Perl code and evals it twice
        $comment =~ s/ ~([a-z_]+(?=[ \]]))/$replacement/eeg;
    }

    print $comment;
    $last_val_name = undef; # reset
}

sub handle_val {
    my $val = shift(@_);
    my $name = shift(@_);

    my $replacement = '$keep_labels{"$name:$1"} ? " $1:$2" : " $2"';

    # /ee treats $replacement as Perl code and evals it twice
    $val =~ s/ ([a-z_]+):([A-Za-z\('])/$replacement/eeg;

    print $val;

    # keep state for doc comments
    $last_val_name = $name;
}

sub handle_def {
    my $def = shift(@_);

    # Unconditionally replace labels in external or type definitions

    # /ee treats $replacement as Perl code and evals it twice
    $def =~ s/ ([a-z_]+):([A-Za-z\('])/ \2/g;

    print $def;
}

sub process {
    my $s = shift(@_);

    while ($s) {
        if ($s =~ /\A\(\*\*([^*]|\*[^)])*\*\)/m) {
            $s = $'; handle_doc_comment($&);
        } elsif ($s =~ /\A\(\*([^*]|\*[^)])*\*\)/m) {
            $s = $'; handle_normal_comment($&);
        } elsif ($s =~ /\A\s+/m) {
            $s = $'; handle_whitespace($&);
        } elsif ($s =~ /\A(val|type|external)\s+/) {
            my $head = $&;
            my $rest = $';
            my $def;
            # split at the next comment or "val|type|external".
            if ($rest =~ /\(\*\*|((val|type|external)\s+)/m) {
                $def = "$head$`";
                $s = "$&$'";
            } else {
                $def = "$head$rest";
                $s = "";
            }
            if ($def =~ /\Aval\s+([a-zA-Z0-9_]+)/) {
                handle_val($def, $1);
            } else {
                handle_def($def);
            }
        } else {
            die "Unrecoginized token";
        }
    }
}

# process STDIN and output
my @lines = <STDIN>;
my $s = join("", @lines);
process($s);
