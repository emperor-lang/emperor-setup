#include "emperor-setup.h"
#include "emperor-setup-args.h"

int main(int argc, char** argv)
{
	args_t* args = parseArgs(argc, argv);

	int totalFlags = args->cFlags + args->libs + args->binaryInstallLocation + args->libraryInstallLocation
	    + args->dataInstallLocation;

	// Ensure the correct number of arguments
	if (totalFlags != 1)
	{
		fprintf(stderr, "Please use one flag per call, use '%s -h' for more information\n", argv[0]);
		free(args);
		exit(1);
	}

	// Output the correct set of flags
	if (args->cFlags)
	{
		printf("%s\n", cFlags());
	}
	else if (args->libs)
	{
		printf("%s\n", libs());
	}
	else if (args->binaryInstallLocation)
	{
		printf("%s\n", binaryInstallLocation());
	}
	else if (args->libraryInstallLocation)
	{
		printf("%s\n", libraryInstallLocation());
	}
	else if (args->dataInstallLocation)
	{
		printf("%s\n", dataInstallLocation());
	}

	free(args);

	return 0;
}

const char* cFlags()
{
	// -std=c18
	const char* cFlags = "-Wall -Werror -Wpedantic -pedantic-errors -I . -I /usr/include/emperor/ -O3 -g";
	return cFlags;
}

const char* libs()
{
	static const char libs[] = "-l pcre";
	return libs;
}

const char* binaryInstallLocation()
{
	const char* binaryInstallLocation = "/usr/bin/";
	return binaryInstallLocation;
}
const char* libraryInstallLocation()
{
	const char* libraryInstallLocation = "/usr/lib/emperor/";
	return libraryInstallLocation;
}
const char* dataInstallLocation()
{
	const char* dataInstallLocation = "/usr/share/emperor/";
	return dataInstallLocation;
}
