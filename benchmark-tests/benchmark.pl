#!/usr/bin/perl

use Time::HiRes qw/time/;

sub fib1 {$_[0] < 2 ? $_[0] : fib1($_[0] - 1) + fib1($_[0] - 2)}

=begin
 CPS-converted fib.
 To pull this off, we first express the recursive fib:

 fib n = n < 2 ? n : fib (n - 1) + fib (n - 2)

 in CPS:

 fib n = n < 2 ? n : fib (n - 1) (i -> fib (n - 2) (j -> k (i + j)))

 Splitting the conditional:

 fib n k | n < 2  = k n
 fib n k | n >= 2 = fib (n - 1) (i [k n] -> fib (n - 2) (j [k i] -> k (i + j)))
=cut

sub fib2 {
  my @stack = ();
  my $n     = $_[0];
  my $i     = 0;
  my $j     = 0;
  my $k     = 'k';
  push @stack, $k, $n;

fib:
  $n = pop @stack;
  $k = pop @stack, push @stack, $n and goto $k if $n < 2;       # Terminating case
  push @stack, $n, 'i', $n - 1;                                 # Send result to first inductive continuation
  goto fib;

i:
  $i = pop @stack;
  $n = pop @stack;
  push @stack, $i, 'j', $n - 2;                                 # Send result to second inductive continuation
  goto fib;

j:
  $j  = pop @stack;
  $i  = pop @stack;
  $k  = pop @stack;
  push @stack, $i + $j;
  goto $k;

k:
  pop @stack;
}

my @conses;
sub cons1 {bless \@_, 'cons'}
sub cons2 {push @conses, @_}

sub timescons1 {cons1(3, 4) for 0 .. shift}
sub timescons2 {cons2(3, 4) for 0 .. shift}

sub timefunction {
  my ($name, $f, @xs) = @_;
  my $xs = join ', ', @xs;
  my $start = time;
  my $x = $f->(@xs);
  my $duration = time - $start;
  print "$name($xs) produced $x and took $duration seconds.\n";
}

timefunction 'rec', \&fib1, 4;
timefunction 'rec', \&fib1, 8;
timefunction 'rec', \&fib1, 16;
timefunction 'rec', \&fib1, 24;
timefunction 'rec', \&fib1, 26;

timefunction 'cps', \&fib2, 4;
timefunction 'cps', \&fib2, 8;
timefunction 'cps', \&fib2, 16;
timefunction 'cps', \&fib2, 24;
timefunction 'cps', \&fib2, 26;

timefunction 'cons1', \&timescons1, 1000000;
timefunction 'cons2', \&timescons2, 1000000;

timefunction 'cons1', \&timescons1, 2000000;
timefunction 'cons2', \&timescons2, 2000000;
