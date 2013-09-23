#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <sys/time.h>

#include <pcre.h>

int
main ()
{
  char const *err;
  int erroff;
  pcre_extra *extra;
  pcre *re;
  
  re = pcre_compile ("a*", 0, &err, &erroff, 0);
  assert (!err);
  extra = pcre_study (re, 0, &err);
  assert (!err);

  int length;
  for (length = 1024; length <= 50 * 1024; length += 1024)
    {
      int i;
      char *in;
      struct timeval min = { 1000, 0 };

      in = malloc (length + 1);
      memset (in, 'a', length);
      in[length] = 0;

      for (i = 0; i < 1000; i++)
        {
          struct timeval start, end, diff;
          gettimeofday (&start, 0);
          pcre_exec (re, extra, in, length, 0, 0, 0, 0);
          gettimeofday (&end, 0);
          assert (!err);

          timersub (&end, &start, &diff);
          if (timercmp (&diff, &min, <))
            {
              min = diff;
              printf ("%d,%d.%06d\r", length / 1024, min.tv_sec, min.tv_usec);
            }
        }

      printf ("%d,%d.%06d\n", length / 1024, min.tv_sec, min.tv_usec);
    }

  pcre_free_study (extra);
  pcre_free (re);
}
