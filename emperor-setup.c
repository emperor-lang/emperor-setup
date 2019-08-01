#include "emperor-setup.h"

/**
 * @brief Entry-point for emperor-setup.
 *
 * Calls argument parser then gives _appropriate_ output.
 *
 * @param argc	The number of command-line arguments
 * @param argv	The contents of the command-lint arguments
 * @return int
 */
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

/**
 * @brief Return the gcc flags to use for compilation
 * 
 * @return const char* 
 */
const char* cFlags()
{
	// -std=c18
	const char* cFlags = "-Wall -Werror -Wpedantic -pedantic-errors -I . -I /usr/include/emperor/ -O3 -g";
	return cFlags;
}

/**
 * @brief Return the C libraries required
 * 
 * @return const char* 
 */
const char* libs()
{
	static const char libs[] = "-l pcre";
	return libs;
}

/**
 * @brief Return the location where compiled programs may be installed
 * 
 * @return const char* 
 */
const char* binaryInstallLocation()
{
	const char* binaryInstallLocation = "/usr/bin/";
	return binaryInstallLocation;
}

/**
 * @brief Return a location where libraries may be installed
 * 
 * @return const char* 
 */
const char* libraryInstallLocation()
{
	const char* libraryInstallLocation = "/usr/lib/emperor/";
	return libraryInstallLocation;
}

/**
 * @brief Return the location where program config data may be installed
 * 
 * @return const char* 
 */
const char* dataInstallLocation()
{
	const char* dataInstallLocation = "/usr/share/emperor/";
	return dataInstallLocation;
}
