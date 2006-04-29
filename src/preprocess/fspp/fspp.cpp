// stand-alone DELPH-IN preprocessor (fspp 0.00)
// (C) Bernd Kiefer, Ben Waldron
//

#define FSPP_VERSION 0.00

#include <iomanip>   // format manipulation

#include <ecl.h>
#include <string>
#include <iostream>

extern "C" {
    char *
    ecl_decode_string(cl_object x) {
	return (type_of(x) == t_string) ? (char *) x->string.self : 0;
    }
}

using namespace std;

// ECL initialization function. Boots the ECL engine
int
ecl_initialize(int argc, char **argv) {
    cl_boot(argc, argv);
    return 0;
}

int
preprocessor_initialize(const char *preproc_pathname) {
    cl_object result;
    result = funcall(2
		     , c_string_to_object("fspp::x-read-preprocessor")
		     , make_string_copy(preproc_pathname)
	);
    return 0;
}

/** Preprocess one string with the preprocessor.  The output format was fixed
 * when the preprocessor engine was initialized.
 */
std::string
preprocess(const char *inputstring, const char *format) {
    //cout << "preprocess: " << inputstring;
    cl_object resobj
	= funcall(4
		  , c_string_to_object("fspp::preprocess")
		  , make_string_copy(inputstring)
		  , c_string_to_object(":format")
		  , c_string_to_object(format)
	    );
    char *result = ecl_decode_string(resobj);
    return (result != NULL) ? std::string(result) : std::string();
}

int
help(void) {
    cout << "Usage:\tfspp FSR_FILENAME (OUTPUT_FORMAT)" << endl;
    cout << "\twhere\t- OUTPUT_FORMAT may be one of :smaf :saf :maf";
    cout << endl;
    cout << "\t\t- (stdin input) each sentence must be terminated by CONTROL-Q NEWLINE";
    cout << endl;
    cout << "\t\t- (stdout output) determined by OUTPUT_FORMAT";
    cout << endl;
    return 0;
}

int main(int argc, char **argv) {
    cout << "(FSPP version " << fixed << setprecision(2) << FSPP_VERSION << ")" << endl;
    cout << "(For support please see: http://wiki.delph-in.net)" << endl;

    if (argc < 2)  { help(); return 1; }
    const char *format = (argc < 3) ? ":smaf" : argv[2];


    ecl_initialize(1, argv);
    
    preprocessor_initialize(argv[1]);
    cout << "Initialized" << endl;

    for (;;)
    {
	string input;
	for (;;)
	{
	    char ch;
	    cin.get(ch);
	    if (ch == '\x11') break;
	    input += ch;
	}
	cin.ignore( INT_MAX, '\n' );
	if (input.empty()) break;
	cout << preprocess(input.c_str(), format) << endl;
    }
    return 0;
}

