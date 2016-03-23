/* %Z% %M%      %I% %G%

   Purpose:	open file specified by environment variable and check for errors

   09/01/2000	Larry Romans	Created
*/
#include "GRACEdefs.h"
#include "GRACEprototype.h"

FILE *file_open(const char *envvar, const char *mode)
{
  static char SccsId[] = "%Z% %M%      %I% %G%";
  FILE *fp;
  char *filename;
  filename = getenv(envvar);
  if (!filename) {
    fprintf(stderr, "Problem with environment variable %s (apparently not set)\n", envvar);
    exit(1);
  }
  fp = fopen(filename, mode);
  if (!fp) {
    fprintf(stderr,
      "Problem opening file %s with mode \"%s\" (from environment variable %s)\n",
      filename, mode, envvar);
    exit(1);
  }
  return(fp);
}

