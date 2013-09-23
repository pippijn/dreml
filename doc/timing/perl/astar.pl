#!/usr/bin/env perl

use common::sense;
use Time::HiRes 'gettimeofday';

$|++;

for my $c (1 .. 50) {
   my $in = "a" x ($c * 1024);

   my ($min, $i) = (100, 0);
   for my $i (1 .. 10000) {
      my $s = gettimeofday;
      $in =~ /a*/;
      my $e = gettimeofday;
      if ($min > $e - $s) {
         $min = $e - $s;
      }
   }
   printf "$c,%.06f\n", $min;
}
