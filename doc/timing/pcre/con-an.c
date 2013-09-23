#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <sys/time.h>

#include <pcre.h>

int
main ()
{
  int length;
  for (length = 1000; length <= 50000; length += 1000)
    {
      int i;
      char *re;
      char const *err;
      int erroff;
      struct timeval min = { 1000, 0 };

      re = malloc (length + 1);
      memset (re, 'a', length);
      re[length] = 0;

      for (i = 0; i < 100; i++)
        {
          struct timeval start, end, diff;
          gettimeofday (&start, 0);
          pcre_free (pcre_compile (re, 0, &err, &erroff, 0));
          gettimeofday (&end, 0);
          assert (!err);

          timersub (&end, &start, &diff);
          if (timercmp (&diff, &min, <))
            {
              min = diff;
              printf ("%d,%d.%06d\r", length, min.tv_sec, min.tv_usec);
            }
        }

      printf ("%d,%d.%06d\n", length, min.tv_sec, min.tv_usec);

      free (re);
    }
}
