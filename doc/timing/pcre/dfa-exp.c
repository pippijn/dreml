#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <sys/time.h>

#include <pcre.h>

int
main ()
{
  int count;
  for (count = 1; count <= 50; count++)
    {
      int length = 6 + 4 * count + 1;
      int i;
      char *re;
      char const *err;
      int erroff;
      struct timeval min = { 1000, 0 };

      re = malloc (length);
      strcpy (re, "[ab]*a");
      for (i = 0; i < count; i++)
        strcat (re, "[ab]");

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
              printf ("%d,%d.%06d\r", count, min.tv_sec, min.tv_usec);
            }
        }

      printf ("%d,%d.%06d\n", count, min.tv_sec, min.tv_usec);

      free (re);
    }
}
