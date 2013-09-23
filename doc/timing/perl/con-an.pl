#!/usr/bin/env perl

use common::sense;
use Time::HiRes 'gettimeofday';

$|++;

for my $c (1 .. 50) {
   my $re = "a" x ($c * 100000);

   my ($min, $i) = (100, 0);
   for my $i (1 .. 100) {
      my $s = gettimeofday;
      "a" =~ /$re/;
      my $e = gettimeofday;
      if ($min > $e - $s) {
         $min = $e - $s;
      }
   }
   printf "$c,%.06f\n", $min;
}
