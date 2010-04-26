#! /usr/bin/perl
#  started 9:11
#  double->double == final price -> payoff
use strict;
use warnings;

use List::Util qw(max min);

sub selloption {
   my $strike = 60;
   my $final = shift;
   return max(0, $strike - $final);
}

sub classoption {
   my $strike = 95;
   my $final = shift;
   return max(0, $strike - $final);
}

my %factMemo;
sub fact {
   my $count = shift;
   if (exists $factMemo{$count}) {
      return $factMemo{$count};
   }
   my $sum = 1;
   for (1..$count) {
      $sum *= $_;
   }
   $factMemo{$count} = $sum;
   return $sum;

}
sub nchoosek {
   my $n = shift;
   my $k = shift;
   return fact($n) / fact($k) * fact($n - $k);
}

sub optionValue {
   my ($periods, $s0, $u, $d, $r, $option) = @_;

   my $p = (1 / (1 + $r) - $d ) / ($u - $d);
   my $q = 1 - $p;

   my $sum = 0;
   for (0..$periods) {
      my $left = ($p ** $_) * ($q ** ($periods - $_));
      my $value = $option->($s0 * ($u ** $_) * ($d ** ($periods - $_)));
      $sum += $left * nchoosek($periods, $_) * $value;
   }
   return ((1 / $r) ** $periods) * $sum;
}

my $runs = 2;
#my $val = optionValue($runs, 50, 1.004, 1 / 1.004, 1.0001, \&selloption);
my $val = optionValue(2, 75, 6/5, 4/5, 11/10, \&classoption);
print "$val\n";
