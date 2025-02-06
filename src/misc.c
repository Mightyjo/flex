/* misc - miscellaneous flex routines */

/*  Copyright (c) 1990 The Regents of the University of California. */
/*  All rights reserved. */

/*  This code is derived from software contributed to Berkeley by */
/*  Vern Paxson. */

/*  The United States Government has rights in this work pursuant */
/*  to contract no. DE-AC03-76SF00098 between the United States */
/*  Department of Energy and the University of California. */

/*  This file is part of flex. */

/*  Redistribution and use in source and binary forms, with or without */
/*  modification, are permitted provided that the following conditions */
/*  are met: */

/*  1. Redistributions of source code must retain the above copyright */
/*     notice, this list of conditions and the following disclaimer. */
/*  2. Redistributions in binary form must reproduce the above copyright */
/*     notice, this list of conditions and the following disclaimer in the */
/*     documentation and/or other materials provided with the distribution. */

/*  Neither the name of the University nor the names of its contributors */
/*  may be used to endorse or promote products derived from this software */
/*  without specific prior written permission. */

/*  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR */
/*  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR */
/*  PURPOSE. */
#include "flexdef.h"
#include "tables.h"
#include "skeletons.h"

/* Append "new_text" to the running buffer. */
void add_action (const char *new_text)
{
	int     len = (int) strlen (new_text);

	while (len + action_index >= action_size - 10 /* slop */ ) {

		if (action_size > INT_MAX / 2)
			/* Increase just a little, to try to avoid overflow
			 * on 16-bit machines.
			 */
			action_size += action_size / 8;
		else
			action_size = action_size * 2;

		action_array =
			reallocate_character_array (action_array,
						    action_size);
	}

	strcpy (&action_array[action_index], new_text);

	action_index += len;
}


/* allocate_array - allocate memory for an integer array of the given size */

void   *allocate_array (int size, size_t element_size)
{
	return reallocate_array(NULL, size, element_size);
}


/* all_lower - true if a string is all lower-case */

int all_lower (char *str)
{
	while (*str) {
		if (!isascii ((unsigned char) * str) || !islower ((unsigned char) * str))
			return 0;
		++str;
	}

	return 1;
}


/* all_upper - true if a string is all upper-case */

int all_upper (char *str)
{
	while (*str) {
		if (!isascii ((unsigned char) * str) || !isupper ((unsigned char) * str))
			return 0;
		++str;
	}

	return 1;
}


/* intcmp - compares two integers for use by qsort. */

int intcmp (const void *a, const void *b)
{
  return *(const int *) a - *(const int *) b;
}


/* check_char - checks a character to make sure it's within the range
 *		we're expecting.  If not, generates fatal error message
 *		and exits.
 */

void check_char (int c)
{
	if (c >= CSIZE)
		lerr (_("bad character '%s' detected in check_char()"),
			readable_form (c));

	if (c >= ctrl.csize)
		lerr (_
			("scanner requires -8 flag to use the character %s"),
			readable_form (c));
}



/* clower - replace upper-case letter to lower-case */

unsigned char clower (int c)
{
	return (unsigned char) ((isascii (c) && isupper (c)) ? tolower (c) : c);
}


char *xstrdup(const char *s)
{
	char *s2;

	if ((s2 = strdup(s)) == NULL)
		flexfatal (_("memory allocation failure in xstrdup()"));

	return s2;
}


/* cclcmp - compares two characters for use by qsort with '\0' sorting last. */

int cclcmp (const void *a, const void *b)
{
	if (!*(const unsigned char *) a)
		return 1;
	else
		if (!*(const unsigned char *) b)
			return - 1;
		else
			return *(const unsigned char *) a - *(const unsigned char *) b;
}


/* dataend - finish up a block of data declarations */

void dataend (const int endit)
{
	const struct flex_backend_t *backend = get_backend();

	/* short circuit any output */
	if (gentables) {

		/* Pretty print the end of the current data line. */
		if (datapos > 0)
			dataflush ();

		/* Optionally, close the table. */
		if (endit)
			backend->close_table(backend);
	}
	dataline = 0;
	datapos = 0;
}


/* dataflush - flush generated data statements */

void dataflush (void)
{
	const struct flex_backend_t *backend = get_backend();

	if (!gentables)
		return;
	
	if (datapos > 0)
		backend->newline(backend);

	if (++dataline >= NUMDATALINES) {
		/* Put out a blank line so that the table is grouped into
		 * large blocks that enable the user to find elements easily.
		 */
		backend->newline(backend);
		dataline = 0;
	}

	/* Reset the number of characters written on the current line. */
	datapos = 0;
}


/* flexerror - report an error message and terminate */

void flexerror (const char *msg)
{
	fprintf (stderr, "%s: %s\n", program_name, msg);
	flexend (1);
}


/* flexfatal - report a fatal error message and terminate */

void flexfatal (const char *msg)
{
	fprintf (stderr, _("%s: fatal internal error, %s\n"),
		 program_name, msg);
	FLEX_EXIT (1);
}


/* lerr - report an error message */

void lerr (const char *msg, ...)
{
	char    errmsg[MAXLINE];
	va_list args;

	va_start(args, msg);
	vsnprintf (errmsg, sizeof(errmsg), msg, args);
	va_end(args);
	flexerror (errmsg);
}


/* lerr_fatal - as lerr, but call flexfatal */

void lerr_fatal (const char *msg, ...)
{
	char    errmsg[MAXLINE];
	va_list args;
	va_start(args, msg);

	vsnprintf (errmsg, sizeof(errmsg), msg, args);
	va_end(args);
	flexfatal (errmsg);
}


/* line_directive_out - spit out a "#line" statement or equivalent */
void line_directive_out (FILE *output_file, char *path, int linenum)
{
	const struct flex_backend_t *backend = get_backend();

	backend->line_directive_out(backend, output_file, path, linenum);
}


/* mark_defs1 - mark the current position in the action array as
 *               representing where the user's section 1 definitions end
 *		 and the prolog begins
 */
void mark_defs1 (void)
{
	defs1_offset = 0;
	action_array[action_index++] = '\0';
	action_offset = prolog_offset = action_index;
	action_array[action_index] = '\0';
}


/* mark_prolog - mark the current position in the action array as
 *               representing the end of the action prolog
 */
void mark_prolog (void)
{
	action_array[action_index++] = '\0';
	action_offset = action_index;
	action_array[action_index] = '\0';
}


/* mk2data - generate a data statement for a two-dimensional array
 *
 * Generates a data statement initializing the current 2-D array to "value".
 */
void mk2data (int value)
{
	const struct flex_backend_t *backend = get_backend();

	/* short circuit any output */
	if (!gentables)
		return;

	if (datapos >= NUMDATAITEMS) {
		backend->column_separator(backend);
		dataflush ();
	}

	if (datapos == 0)
		backend->indent(backend);
	else
	  backend->column_separator(backend);

	++datapos;

	backend->format_data_table_entry(backend, value);
}


/* mkdata - generate a data statement
 *
 * Generates a data statement initializing the current array element to
 * "value".
 */
void mkdata (int value)
{
	const struct flex_backend_t *backend = get_backend();

	/* short circuit any output */
	if (!gentables)
		return;

	if (datapos >= NUMDATAITEMS) {
		backend->column_separator(backend);
		dataflush ();
	}

	if (datapos == 0)
		backend->indent(backend);
	else
	  backend->column_separator(backend);

	++datapos;

	backend->format_data_table_entry(backend, value);
}


/* myctoi - return the integer represented by a string of digits */

int myctoi (const char *array)
{
	int     val = 0;

	(void) sscanf (array, "%d", &val);

	return val;
}


/* myesc - return character corresponding to escape sequence */

unsigned char myesc (unsigned char array[])
{
	unsigned char    c, esc_char;

	switch (array[1]) {
	case 'b':
		return '\b';
	case 'f':
		return '\f';
	case 'n':
		return '\n';
	case 'r':
		return '\r';
	case 't':
		return '\t';
	case 'a':
		return '\a';
	case 'v':
		return '\v';
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		{		/* \<octal> */
			int     sptr = 1;

			while (sptr <= 3 &&
                               array[sptr] >= '0' && array[sptr] <= '7') {
				++sptr;
			}

			c = array[sptr];
			array[sptr] = '\0';

			esc_char = (unsigned char) strtoul (array + 1, NULL, 8);

			array[sptr] = c;

			return esc_char;
		}

	case 'x':
		{		/* \x<hex> */
			int     sptr = 2;

			while (sptr <= 3 && isxdigit (array[sptr])) {
				/* Don't increment inside loop control
				 * because if isxdigit() is a macro it might
				 * expand into multiple increments ...
				 */
				++sptr;
			}

			c = array[sptr];
			array[sptr] = '\0';

			esc_char = (unsigned char) strtoul (array + 2, NULL, 16);

			array[sptr] = c;

			return esc_char;
		}

	default:
		return array[1];
	}
}


/* readable_form - return the the human-readable form of a character
 *
 * The returned string is in static storage.
 */

char   *readable_form (int c)
{
	static char rform[20];

	if ((c >= 0 && c < 32) || c >= 127) {
		switch (c) {
		case '\b':
			return "\\b";
		case '\f':
			return "\\f";
		case '\n':
			return "\\n";
		case '\r':
			return "\\r";
		case '\t':
			return "\\t";
		case '\a':
			return "\\a";
		case '\v':
			return "\\v";
		default:
			if(env.trace_hex)
				snprintf (rform, sizeof(rform), "\\x%.2x", (unsigned int) c);
			else
				snprintf (rform, sizeof(rform), "\\%.3o", (unsigned int) c);
			return rform;
		}
	}

	else if (c == ' ')
		return "' '";

	else {
		rform[0] = (char) c;
		rform[1] = '\0';

		return rform;
	}
}


/* reallocate_array - increase the size of a dynamic array */

void   *reallocate_array (void *array, int size, size_t element_size)
{
	void *new_array;
#ifdef HAVE_REALLOCARR
	new_array = array;
	if (reallocarr(&new_array, (size_t) size, element_size)) {
		flexfatal ((array) ?
			_("attempt to increase array size failed") :
			/* Function name is allocate_array() because of
			 * compatibility (for translations): */
			_("memory allocation failed in allocate_array()"));
	}
#else
# ifdef HAVE_REALLOCARRAY
	new_array = reallocarray(array, (size_t) size, element_size);
# else
	/* Do manual overflow detection */
	size_t num_bytes = (size_t) size * element_size;
	new_array = (size && SIZE_MAX / (size_t) size < element_size) ? NULL :
		realloc(array, num_bytes);
# endif
	if (!new_array) {
		flexfatal ((array) ?
			_("attempt to increase array size failed") :
			/* Function name is allocate_array() because of
			 * compatibility (for translations): */
			_("memory allocation failed in allocate_array()"));
	}
#endif
	return new_array;
}


/* transition_struct_out - output a yy_trans_info structure
 *
 * outputs the yy_trans_info structure with the two elements, element_v and
 * element_n.  Formats the output with spaces and carriage returns.
 */

void transition_struct_out (int element_v, int element_n)
{
	const struct flex_backend_t *backend = get_backend();

	/* short circuit any output */
	if (!gentables)
		return;

	backend->open_table(backend);
	backend->format_data_table_entry(backend, element_v);
	backend->column_separator(backend);
	backend->format_data_table_entry(backend, element_n);
	backend->continue_table(backend);
	backend->newline(backend);

	datapos += TRANS_STRUCT_PRINT_LENGTH;

	if (datapos >= 79 - TRANS_STRUCT_PRINT_LENGTH) {
		backend->newline(backend);

		if (++dataline % 10 == 0)
			backend->newline(backend);

		datapos = 0;
	}
}


/* The following is only needed when building flex's parser using certain
 * broken versions of bison.
 *
 * XXX: this is should go soon
 */
void   *yy_flex_xmalloc (int size)
{
	void   *result;

	result = malloc((size_t) size);
	if (!result)
		flexfatal (_
			   ("memory allocation failed in yy_flex_xmalloc()"));

	return result;
}


/* Remove all '\n' and '\r' characters, if any, from the end of str.
 * str can be any null-terminated string, or NULL.
 * returns str. */
char   *chomp (char *str)
{
	char   *p = str;

	if (!str || !*str)	/* s is null or empty string */
		return str;

	/* find end of string minus one */
	while (*p)
		++p;
	--p;

	/* eat newlines */
	while (p >= str && (*p == '\r' || *p == '\n'))
		*p-- = 0;
	return str;
}


