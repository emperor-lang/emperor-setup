#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char help[] = "Hello, world!";
static const char cFlags[] = "-Wall -Werror -Wpedantic -pedantic-errors -O3 -g";
static const char cLibs[] = "";

int main(int argc, char **argv)
{
	// Check sufficient arguments have been given
	if (argc <= 1)
	{
		fprintf(stderr, "%s\n", help);
		exit(-1);
	}

	// Output gcc flags for each mapping
	for (int i = 1; i < argc; i++)
	{
		if (strcmp("-h", argv[i]) == 0 || strcmp("--help", argv[i]) == 0)
		{
			printf("%s\n", help);
		}
		else if (strcmp("--cflags", argv[i]) == 0)
		{
			printf("%s\n", cFlags);
		}
		// else if (strcmp("--profiler", argv[i]) == 0)
		// {
		// }
		else if (strcmp("--clibs", argv[i]) == 0)
		{
			printf("%s\n", cLibs);
		}
		else
		{
			fprintf(stderr, "%s %s\n", "Unrecognised argument:", argv[i]);
			exit(-1);
		}
	}

	return 0;
}