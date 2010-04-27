#! /usr/bin/perl
use strict;
use warnings;
use POSIX;
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

my %choosehash;
sub nchoosek {
   my $n = shift;
   my $k = shift;
   $k > $n and return 0;
   if ($k > floor($n / 2)) {
      $k = $n - $k;
   }
   my $key = "n" . $n . "k" . $k;
   if (exists $choosehash{$key}) {
      return $choosehash{$key};
   }
   my $sum = 1;
   for (1..$k) {
      $sum *= $n--;
      $sum /= $_;
   }
   $choosehash{$key} = $sum;
   return $sum;
}

sub optionValue {
   my ($periods, $s0, $u, $d, $r, $option) = @_;

   my $p = ($r - $d ) / ($u - $d);
   my $q = 1 - $p;

   my $sum = 0;
   for (0..$periods) {
      my $left = ($p ** $_) * ($q ** ($periods - $_));
      my $value = $option->($s0 * ($u ** $_) * ($d ** ($periods - $_)));
      $sum += $left * nchoosek($periods, $_) * $value;
   }

   return ((1 / $r) ** $periods) * $sum;
}

my $runs = 1020;
for (1..980) {
   optionValue($runs, 50, 1.004, 1 / 1.004, 1.0001, \&selloption);
}
#my $val = optionValue(2, 75, 6/5, 4/5, 11/10, \&classoption);
