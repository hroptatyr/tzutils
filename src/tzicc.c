/*** tzicc.c -- zoneinfo to ical converter
 *
 * Copyright (C) 2014-2015 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <freundt@ga-group.nl>
 *
 * This file is part of tzutils.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of any contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ** Based on zic.c:
 * This file is in the public domain, so clarified as of
 * 2006-07-17 by Arthur David Olson. */
#if defined HAVE_CONFIG_H
# include "config.h"
#endif	/* HAVE_CONFIG_H */
#if defined HAVE_VERSION_H
# include "version.h"
#endif	/* HAVE_VERSION_H */
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <assert.h>
#include <stdint.h>
#include <inttypes.h>
#include "tzfile.h"
#include "intern.h"
#include "nifty.h"

#define _(x)	(x)

/* line codes */
typedef enum {
	LC_RULE = 0U,
	LC_ZONE = 1U,
	LC_LINK = 2U,
	LC_LEAP = 3U,
} zic_lc_t;

/* year synonyms */
typedef enum {
	YR_MINIMUM = 0U,
	YR_MAXIMUM = 1U,
	YR_ONLY = 2U,
} zic_yr_t;


struct lookup {
	const char *l_word;
	const unsigned int l_value;
};

static struct lookup const line_codes[] = {
	{"Rule", LC_RULE},
	{"Zone", LC_ZONE},
	{"Link", LC_LINK},
	{"Leap", LC_LEAP},
	{NULL, 0}
};

static struct lookup const mon_names[] = {
	{"January", TM_JANUARY},
	{"February", TM_FEBRUARY},
	{"March", TM_MARCH},
	{"April", TM_APRIL},
	{"May", TM_MAY},
	{"June", TM_JUNE},
	{"July", TM_JULY},
	{"August", TM_AUGUST},
	{"September", TM_SEPTEMBER},
	{"October", TM_OCTOBER},
	{"November", TM_NOVEMBER},
	{"December", TM_DECEMBER},
	{NULL, 0}
};

static struct lookup const wday_names[] = {
	{"Sunday", TM_SUNDAY},
	{"Monday", TM_MONDAY},
	{"Tuesday", TM_TUESDAY},
	{"Wednesday", TM_WEDNESDAY},
	{"Thursday", TM_THURSDAY},
	{"Friday", TM_FRIDAY},
	{"Saturday", TM_SATURDAY},
	{NULL, 0}
};

static struct lookup const lasts[] = {
	{"last-Sunday", TM_SUNDAY},
	{"last-Monday", TM_MONDAY},
	{"last-Tuesday", TM_TUESDAY},
	{"last-Wednesday", TM_WEDNESDAY},
	{"last-Thursday", TM_THURSDAY},
	{"last-Friday", TM_FRIDAY},
	{"last-Saturday", TM_SATURDAY},
	{NULL, 0}
};

static struct lookup const begin_years[] = {
	{"minimum", YR_MINIMUM},
	{"maximum", YR_MAXIMUM},
	{NULL, 0}
};

static struct lookup const end_years[] = {
	{"minimum", YR_MINIMUM},
	{"maximum", YR_MAXIMUM},
	{"only", YR_ONLY},
	{NULL, 0}
};

static struct lookup const leap_types[] = {
	{"Rolling", true},
	{"Stationary", false},
	{NULL, 0}
};

typedef int_fast64_t zic_t;
#define ZIC_MIN INT_FAST64_MIN
#define ZIC_MAX INT_FAST64_MAX
#define PRIdZIC	PRIdFAST64

#define TIME_T_BITS_IN_FILE	64
static const zic_t min_time = (zic_t)-1 << (TIME_T_BITS_IN_FILE - 1);
static const zic_t max_time = -1 - ((zic_t)-1 << (TIME_T_BITS_IN_FILE - 1));

/* r_dycode			r_dayofmonth	r_wday */
#define DC_DOM		0	/* 1..31 */	/* unused */
#define DC_DOWGEQ	1	/* 1..31 */	/* 0..6 (Sun..Sat) */
#define DC_DOWLEQ	2	/* 1..31 */	/* 0..6 (Sun..Sat) */

struct rule {
	obint_t r_name;

	zic_t r_loyear;	/* for example, 1986 */
	zic_t r_hiyear;	/* for example, 1986 */
	obint_t r_yrtype;
	bool r_lowasnum;
	bool r_hiwasnum;

	int r_month;	/* 0..11 */

	int r_dycode;	/* see below */
	int r_dayofmonth;
	int r_wday;

	zic_t r_tod;		/* time from midnight */
	bool r_todisstd;	/* above is standard time if 1 */
					/* or wall clock time if 0 */
	bool r_todisgmt;	/* above is GMT if 1 */
					/* or local time if 0 */
	zic_t r_stdoff;	/* offset from standard time */

	int r_todo;		/* a rule to do (used in outzone) */
	zic_t r_temp;		/* used in outzone */
};

/* fields on a Rule line */
#define RF_NAME		1
#define RF_LOYEAR	2
#define RF_HIYEAR	3
#define RF_COMMAND	4
#define RF_MONTH	5
#define RF_DAY		6
#define RF_TOD		7
#define RF_STDOFF	8
#define RF_ABBRVAR	9
#define RULE_FIELDS	10


struct zone {
	obint_t z_name;
	zic_t z_gmtoff;
	obint_t z_rule;

	zic_t z_stdoff;

	struct rule *z_rules;
	size_t z_nrules;

	struct rule z_untilrule;
	zic_t z_untiltime;
};

/* fields on a Zone line */
#define ZF_NAME		1
#define ZF_GMTOFF	2
#define ZF_RULE		3
#define ZF_FORMAT	4
#define ZF_TILYEAR	5
#define ZF_TILMONTH	6
#define ZF_TILDAY	7
#define ZF_TILTIME	8
#define ZONE_MINFIELDS	5
#define ZONE_MAXFIELDS	9

/* fields on a Zone continuation line */
#define ZFC_GMTOFF	0
#define ZFC_RULE	1
#define ZFC_FORMAT	2
#define ZFC_TILYEAR	3
#define ZFC_TILMONTH	4
#define ZFC_TILDAY	5
#define ZFC_TILTIME	6
#define ZONEC_MINFIELDS	3
#define ZONEC_MAXFIELDS	7


static obarray_t zobs;
static obarray_t robs;

static const int len_months[2U][MONSPERYEAR] = {
	{31U, 28U, 31U, 30U, 31U, 30U, 31U, 31U, 30U, 31U, 30U, 31U},
	{31U, 29U, 31U, 30U, 31U, 30U, 31U, 31U, 30U, 31U, 30U, 31U},
};

static const int len_years[2U] = {
	DAYSPERNYEAR, DAYSPERLYEAR
};

static struct rule *rules;
static size_t nrules;
static size_t zrules;

static struct zone *zones;
static size_t nzones;
static size_t zzones;


static void
__attribute__((format(printf, 1, 2)))
error(const char *fmt, ...)
{
	va_list vap;
	va_start(vap, fmt);
	vfprintf(stderr, fmt, vap);
	va_end(vap);
	if (errno) {
		fputc(':', stderr);
		fputc(' ', stderr);
		fputs(strerror(errno), stderr);
	}
	fputc('\n', stderr);
	return;
}

static __attribute__((pure)) char
lowerit(char a)
{
	if (a >= 'A' && a <= 'Z') {
		a += 0x20;
	}
	return a;
}

#if defined __INTEL_COMPILER
# pragma warning (disable:981)
#endif	/* __INTEL_COMPILER */

/* case-insensitive equality */
static __attribute__((pure)) bool
ciequal(register const char *ap, register const char *bp)
{
	while (lowerit(*ap) == lowerit(*bp++)) {
		if (*ap++ <= ' ') {
			return true;
		}
	}
	return false;
}

static __attribute__((pure)) bool
itsabbr(register const char *abbr, register const char *word)
{
	if (lowerit(*abbr) != lowerit(*word)) {
		return false;
	}
	++word;
	while (*++abbr > ' ') {
		do {
			if (*word <= ' ') {
				return false;
			}
		} while (lowerit(*word++) != lowerit(*abbr));
	}
	return true;
}

#if defined __INTEL_COMPILER
# pragma warning (default:981)
#endif	/* __INTEL_COMPILER */

static __attribute__((noreturn)) void
time_overflow(void)
{
	error(_("time overflow"));
	exit(EXIT_FAILURE);
}

static __attribute__((pure)) zic_t
oadd(const zic_t t1, const zic_t t2)
{
	if (t1 < 0 ? t2 < ZIC_MIN - t1 : ZIC_MAX - t1 < t2) {
		time_overflow();
	}
	return t1 + t2;
}

static __attribute__((pure)) zic_t
tadd(const zic_t t1, const zic_t t2)
{
	if (t1 < 0) {
		if (t2 < min_time - t1) {
			if (t1 != min_time) {
				time_overflow();
			}
			return min_time;
		}
	} else {
		if (max_time - t1 < t2) {
			if (t1 != max_time) {
				time_overflow();
			}
			return max_time;
		}
	}
	return t1 + t2;
}

static __attribute__((pure)) const struct lookup*
byword(register const char *const w, register const struct lookup *const table)
{
	register const struct lookup *foundlp;
	register const struct lookup *lp;

	if (w == NULL || table == NULL) {
		return NULL;
	}
	/* look for exact match */
	for (lp = table; lp->l_word != NULL; ++lp) {
		if (ciequal(w, lp->l_word)) {
			return lp;
		}
	}
	/* look for inexact match */
	foundlp = NULL;
	for (lp = table; lp->l_word != NULL; ++lp) {
		if (itsabbr(w, lp->l_word)) {
			if (foundlp == NULL) {
				foundlp = lp;
			} else {
				/* multiple inexact matches */
				return NULL;
			}
		}
	}
	return foundlp;
}

static zic_t
gethms(char const *string, char const *errstring, bool signable)
{
	zic_t hh;
	int mm, ss, sign;
	char *xs;

	if (string == NULL || *string == '\0') {
		return 0;
	}
	if (!signable) {
		sign = 1;
	}
	else if (*string == '-') {
		sign = -1;
		++string;
	} else {
		sign = 1;
	}
	hh = strtoul(string, &xs, 10);
	if (*xs != ':') {
		mm = ss = 0;
	} else {
		mm = strtoul(++xs, &xs, 10);
		if (*xs != ':') {
			ss = 0;
		} else {
			ss = strtoul(++xs, &xs, 10);
		}
	}
	if (hh < 0 ||
	    mm < 0 || mm >= MINSPERHOUR ||
	    ss < 0 || ss > SECSPERMIN) {
		error("%s", errstring);
		return 0;
	}
	if (ZIC_MAX / SECSPERHOUR < hh) {
		error(_("time overflow"));
		return 0;
	}
	return oadd(sign * hh * SECSPERHOUR,
		    sign * (mm * SECSPERMIN + ss));
}

static zic_t
rpytime(register const struct rule *const rp, register const zic_t wantedy)
{
/*
** Given a rule, and a year, compute the date (in seconds since January 1,
** 1970, 00:00 LOCAL time) in that year that the rule refers to.
*/
	register int m, i;
	register zic_t dayoff;
	register zic_t t, y;

	if (wantedy == ZIC_MIN) {
		return min_time;
	}
	if (wantedy == ZIC_MAX) {
		return max_time;
	}
	dayoff = 0;
	m = TM_JANUARY;
	y = EPOCH_YEAR;
	while (wantedy != y) {
		if (wantedy > y) {
			i = len_years[isleap(y)];
			++y;
		} else {
			--y;
			i = -len_years[isleap(y)];
		}
		dayoff = oadd(dayoff, i);
	}
	while (m != rp->r_month) {
		i = len_months[isleap(y)][m];
		dayoff = oadd(dayoff, i);
		++m;
	}
	i = rp->r_dayofmonth;
	if (m == TM_FEBRUARY && i == 29 && !isleap(y)) {
		if (rp->r_dycode == DC_DOWLEQ) {
			--i;
		} else {
			error(_("use of 2/29 in non leap-year"));
			exit(EXIT_FAILURE);
		}
	}
	i--;
	dayoff = oadd(dayoff, i);
	if (rp->r_dycode == DC_DOWGEQ || rp->r_dycode == DC_DOWLEQ) {
		register zic_t	wday;

#define LDAYSPERWEEK	((zic_t) DAYSPERWEEK)
		wday = EPOCH_WDAY;
		/*
		** Don't trust mod of negative numbers.
		*/
		if (dayoff >= 0) {
			wday = (wday + dayoff) % LDAYSPERWEEK;
		} else {
			wday -= ((-dayoff) % LDAYSPERWEEK);
			if (wday < 0) {
				wday += LDAYSPERWEEK;
			}
		}
		while (wday != rp->r_wday) {
			if (rp->r_dycode == DC_DOWGEQ) {
				dayoff = oadd(dayoff, 1);
				if (++wday >= LDAYSPERWEEK) {
					wday = 0;
				}
				++i;
			} else {
				dayoff = oadd(dayoff, -1);
				if (--wday < 0) {
					wday = LDAYSPERWEEK - 1;
				}
				--i;
			}
		}
	}
	if (dayoff < min_time / SECSPERDAY) {
		return min_time;
	}
	if (dayoff > max_time / SECSPERDAY) {
		return max_time;
	}
	t = (zic_t) dayoff * SECSPERDAY;
	return tadd(t, rp->r_tod);
}

static void
rulesub(register struct rule *const rp,
	const char *const loyearp,
	const char *const hiyearp,
	const char *const typep,
	const char *const monthp,
	const char *const dayp,
	const char *const timep)
{
	register const struct lookup *lp;
	register const char *cp;
	register char *dp;
	register char *ep;
	char *xs;

	if ((lp = byword(monthp, mon_names)) == NULL) {
		error(_("invalid month name"));
		return;
	}
	rp->r_month = lp->l_value;
	rp->r_todisstd = false;
	rp->r_todisgmt = false;
	dp = strdup(timep);
	if (*dp != '\0') {
		ep = dp + strlen(dp) - 1;
		switch (lowerit(*ep)) {
			case 's':	/* Standard */
				rp->r_todisstd = true;
				rp->r_todisgmt = false;
				*ep = '\0';
				break;
			case 'w':	/* Wall */
				rp->r_todisstd = false;
				rp->r_todisgmt = false;
				*ep = '\0';
				break;
			case 'g':	/* Greenwich */
			case 'u':	/* Universal */
			case 'z':	/* Zulu */
				rp->r_todisstd = true;
				rp->r_todisgmt = true;
				*ep = '\0';
				break;
		}
	}
	rp->r_tod = gethms(dp, _("invalid time of day"), false);
	free(dp);
	/*
	** Year work.
	*/
	cp = loyearp;
	lp = byword(cp, begin_years);
	rp->r_lowasnum = lp == NULL;
	if (!rp->r_lowasnum) switch ((int) lp->l_value) {
		case YR_MINIMUM:
			rp->r_loyear = ZIC_MIN;
			break;
		case YR_MAXIMUM:
			rp->r_loyear = ZIC_MAX;
			break;
		default:	/* "cannot happen" */
			abort();
	} else if (!(rp->r_loyear = strtoul(cp, &xs, 10))) {
		error(_("invalid starting year"));
		return;
	}
	cp = hiyearp;
	lp = byword(cp, end_years);
	rp->r_hiwasnum = lp == NULL;
	if (!rp->r_hiwasnum) switch ((int) lp->l_value) {
		case YR_MINIMUM:
			rp->r_hiyear = ZIC_MIN;
			break;
		case YR_MAXIMUM:
			rp->r_hiyear = ZIC_MAX;
			break;
		case YR_ONLY:
			rp->r_hiyear = rp->r_loyear;
			break;
		default:
			abort();
	} else if (!(rp->r_hiyear = strtoul(cp, &xs, 10))) {
		error(_("invalid ending year"));
		return;
	}
	if (rp->r_loyear > rp->r_hiyear) {
		error(_("starting year greater than ending year"));
		return;
	}
	if (*typep == '\0' || *typep == '-')
		rp->r_yrtype = 0U;
	else {
		if (rp->r_loyear == rp->r_hiyear) {
			error(_("typed single year"));
			return;
		}
		rp->r_yrtype = intern(NULL, typep, 0U);
	}
	/*
	** Day work.
	** Accept things such as:
	**	1
	**	last-Sunday
	**	Sun<=20
	**	Sun>=7
	*/
	dp = strdup(dayp);
	if ((lp = byword(dp, lasts)) != NULL) {
		rp->r_dycode = DC_DOWLEQ;
		rp->r_wday = lp->l_value;
		rp->r_dayofmonth = 31;
	} else {
		if ((ep = strchr(dp, '<')) != 0)
			rp->r_dycode = DC_DOWLEQ;
		else if ((ep = strchr(dp, '>')) != 0)
			rp->r_dycode = DC_DOWGEQ;
		else {
			ep = dp;
			rp->r_dycode = DC_DOM;
		}
		if (rp->r_dycode != DC_DOM) {
			*ep++ = 0;
			if (*ep++ != '=') {
				error(_("invalid day of month"));
				free(dp);
				return;
			}
			if ((lp = byword(dp, wday_names)) == NULL) {
				error(_("invalid weekday name"));
				free(dp);
				return;
			}
			rp->r_wday = lp->l_value;
		}
		if (!(rp->r_dayofmonth = strtoul(ep, &xs, 10)) ||
		    (rp->r_dayofmonth > len_months[1][rp->r_month])) {
				error(_("invalid day of month"));
				free(dp);
				return;
		}
	}
	free(dp);
}

static void
inrule(const char *base, off_t f[static 16U], size_t nf)
{
	static struct rule r;

	if (nf != RULE_FIELDS) {
		error(_("wrong number of fields on Rule line"));
		return;
	} else if (base[f[RF_NAME]] == '\0') {
		error(_("nameless rule"));
		return;
	}
	r.r_stdoff = gethms(base + f[RF_STDOFF], _("invalid saved time"), true);
	rulesub(&r,
		base + f[RF_LOYEAR], base + f[RF_HIYEAR], base + f[RF_COMMAND],
		base + f[RF_MONTH], base + f[RF_DAY], base + f[RF_TOD]);
	r.r_name = intern(robs, base + f[RF_NAME], 0U);
	if (nrules >= zrules) {
		const size_t nuz = (2U * zrules) ?: 64U;
		rules = realloc(rules, nuz * sizeof(*rules));
		zrules = nuz;
	}
	rules[nrules++] = r;
	return;
}

static int
inzsub(const char *base, off_t flds[static 16U], size_t nflds, bool contp)
{
	static struct zone z;
	register char *cp;
	register int i_gmtoff, i_rule, i_format;
	register int i_untilyear, i_untilmonth;
	register int i_untilday, i_untiltime;
	register bool hasuntil;

	if (contp) {
		i_gmtoff = ZFC_GMTOFF;
		i_rule = ZFC_RULE;
		i_format = ZFC_FORMAT;
		i_untilyear = ZFC_TILYEAR;
		i_untilmonth = ZFC_TILMONTH;
		i_untilday = ZFC_TILDAY;
		i_untiltime = ZFC_TILTIME;
	} else {
		i_gmtoff = ZF_GMTOFF;
		i_rule = ZF_RULE;
		i_format = ZF_FORMAT;
		i_untilyear = ZF_TILYEAR;
		i_untilmonth = ZF_TILMONTH;
		i_untilday = ZF_TILDAY;
		i_untiltime = ZF_TILTIME;
		z.z_name = intern(zobs, base + flds[ZF_NAME], 0U);
	}
	z.z_gmtoff = gethms(base + flds[i_gmtoff], _("invalid UT offset"), true);
	if ((cp = strchr(base + flds[i_format], '%')) != 0) {
		if (*++cp != 's' || strchr(cp, '%') != 0) {
			error(_("invalid abbreviation format"));
			return false;
		}
	}
	z.z_rule = intern(robs, base + flds[i_rule], 0U);
	hasuntil = nflds > (size_t)i_untilyear;
	if (hasuntil) {
		rulesub(&z.z_untilrule,
			base + flds[i_untilyear],
			"only",
			"",
			(nflds > (size_t)i_untilmonth) ?
			base + flds[i_untilmonth] : "Jan",
			(nflds > (size_t)i_untilday) ?
			base + flds[i_untilday] : "1",
			(nflds > (size_t)i_untiltime) ?
			base + flds[i_untiltime] : "0");
		z.z_untiltime = rpytime(&z.z_untilrule, z.z_untilrule.r_loyear);
		if (contp && nzones > 0 &&
			z.z_untiltime > min_time &&
			z.z_untiltime < max_time &&
			zones[nzones - 1].z_untiltime > min_time &&
			zones[nzones - 1].z_untiltime < max_time &&
			zones[nzones - 1].z_untiltime >= z.z_untiltime) {
				error(_("\
Zone continuation line end time is not after end time of previous line"));
				return -1;
		}
	}
	if (nzones >= zzones) {
		const size_t nuz = (2U * zzones) ?: 64U;
		zones = realloc(zones, nuz * sizeof(*zones));
		zzones = nuz;
	}
	zones[nzones++] = z;
	/*
	** If there was an UNTIL field on this line,
	** there's more information about the zone on the next line.
	*/
	return hasuntil;
}

static int
inzone(const char *base, off_t flds[static 16U], size_t nflds)
{
	if (nflds < ZONE_MINFIELDS || nflds > ZONE_MAXFIELDS) {
		error(_("wrong number of fields on Zone line"));
		return -1;
	}
	return inzsub(base, flds, nflds, false);
}

static int
inzcont(const char *base, off_t flds[static 16U], size_t nflds)
{
	if (nflds < ZONEC_MINFIELDS || nflds > ZONEC_MAXFIELDS) {
		error(_("wrong number of fields on Zone continuation line"));
		return -1;
	}
	return inzsub(base, flds, nflds, true);
}

static size_t
getfields(off_t flds[static 16U], char *line, size_t llen)
{
	unsigned char *lp = (unsigned char*)line;
	unsigned char *const ep = lp + llen;
	size_t nf = 0U;

	for (;; nf++) {
		/* overread whitespace */
		for (; lp < ep && *lp <= ' '; lp++);

		if (lp >= ep || *lp == '#') {
			break;
		}
		/* otherwise, we've got ourselves a field */
		flds[nf] = lp - (unsigned char*)line;

		if (LIKELY(*lp++ != '"')) {
			/* go to next space */
			for (; lp < ep && *lp > ' '; lp++);
		} else {
			flds[nf]++;
			/* find matching " */
			for (; lp < ep && *lp != '"'; lp++);
		}
		*lp++ = '\0';
	}
	return nf;
}


static int
zic_fn(const char *fn)
{
	FILE *fp;
	char *line = NULL;
	size_t llen = 0UL;
	bool zonecontp = false;

	if (fn == NULL || fn[0U] == '-' && fn[1U] == '\0') {
		/* read from stdin */
		fp = stdin;
	} else if ((fp = fopen(fn, "r")) == NULL) {
		error(_("Fatal: cannot open file `%s'"), fn);
		return -1;
	}

	for (ssize_t nrd; (nrd = getline(&line, &llen, fp)) > 0;) {
		register const struct lookup *lp;
		off_t flds[16U];
		size_t nflds = getfields(flds, line, nrd);

		if (!nflds || line[*flds] == '#') {
			/* comment line */
			continue;
		} else if (zonecontp) {
			/* resume previous zone line */
			if (inzcont(line, flds, nflds) <= 0) {
				zonecontp = false;
			}
			continue;
		} else if ((lp = byword(line + *flds, line_codes)) == NULL) {
			error(_("input line of unknown type"));
			continue;
		}
		zonecontp = false;
		switch ((zic_lc_t)lp->l_value) {
		case LC_RULE:
			inrule(line, flds, nflds);
			break;
		case LC_ZONE:
			if (inzone(line, flds, nflds) > 0) {
				zonecontp = true;
			}
			break;
		case LC_LINK:
			error("Link lines not supported");
			break;
		case LC_LEAP:
			error("Leap lines not supported");
			break;
		}
	}
	if (LIKELY(line != NULL)) {
		free(line);
	}
	fclose(fp);
	return 0;
}

static void
associate(void)
{
/*
** Associate sets of rules with zones.
*/
	auto inline int rcomp(const void *cp1, const void *cp2)
	{
		const struct rule *r1 = cp1;
		const struct rule *r2 = cp2;
		return r1->r_name - r2->r_name;
	}

	if (nrules) {
		/* sort by name */
		qsort(rules, nrules, sizeof(*rules), rcomp);
	}
	/* reset */
	for (size_t i = 0U; i < nzones; ++i) {
		zones[i].z_rules = NULL;
		zones[i].z_nrules = 0U;
	}
	for (size_t base = 0U, out; base < nrules; base = out) {
		for (out = base + 1U; out < nrules; out++) {
			if (rules[base].r_name != rules[out].r_name) {
				break;
			}
		}
		for (size_t i = 0U; i < nzones; i++) {
			if (zones[i].z_rule != rules[base].r_name) {
				continue;
			}
			zones[i].z_rules = rules + base;
			zones[i].z_nrules = out - base;
		}
	}
	for (size_t i = 0U; i < nzones; i++) {
		if (zones[i].z_nrules) {
			/* all good */
			continue;
		}
		/*
		** Maybe we have a local standard time offset.
		*/
		zones[i].z_stdoff =
			gethms(obint_name(robs, zones[i].z_rule),
			       _("unruly zone"), true);
	}
	return;
}

static void
pr_ical_hdr(void)
{
	fputs("\
BEGIN:VCALENDAR\n\
VERSION:2.0\n\
PRODID:-//GA Financial Solutions//echse//EN\n\
CALSCALE:GREGORIAN\n\
", stdout);
	return;
}

static void
pr_ical_ftr(void)
{
	fputs("\
END:VCALENDAR\n\
", stdout);
	return;
}

static void
pr_ical_r(const struct zone z[static 1U], const struct rule r[static 1U])
{
	static const char *wday[DAYSPERWEEK] = {
		"SU", "MO", "TU", "WE", "TH", "FR", "SA",
	};
	int new = z->z_gmtoff + r->r_stdoff;

	if (UNLIKELY(r->r_loyear == ZIC_MIN)) {
		return;
	}

	fputs("BEGIN:VEVENT\n", stdout);

	fprintf(stdout, "\
SUMMARY:Daylight saving transition %s -> UTC%+02d:%02d:%02d\n",
		obint_name(zobs, z->z_name),
		new / 3600, (new / 60) % 60, new % 60);

	fprintf(stdout, "\
DTSTART:%" PRIdZIC "0101T%02" PRIdZIC "%02" PRIdZIC "%02" PRIdZIC "\n",
		r->r_loyear,
		r->r_tod / 3600, (r->r_tod / 60) % 60, r->r_tod % 60);

	fprintf(stdout, "\
RRULE:FREQ=YEARLY;BYMONTH=%d", r->r_month + 1);

	switch (r->r_dycode) {
	case DC_DOM:
		fprintf(stdout, "\
;BYMONTHDAY=%d", r->r_dayofmonth);
		break;
	case DC_DOWGEQ:
		if (!((r->r_dayofmonth - 1) % DAYSPERWEEK)) {
			/* be intelligent about this one */
			fprintf(stdout, "\
;BYDAY=%d%s", (r->r_dayofmonth - 1) / DAYSPERWEEK + 1, wday[r->r_wday]);
		} else {
			fprintf(stdout, "\
;BYDAY=%s;BYMONTHDAY=%d,%d,%d,%d,%d,%d,%d", wday[r->r_wday],
			r->r_dayofmonth + 0,
			r->r_dayofmonth + 1,
			r->r_dayofmonth + 2,
			r->r_dayofmonth + 3,
			r->r_dayofmonth + 4,
			r->r_dayofmonth + 5,
			r->r_dayofmonth + 6);
		}
		break;
	case DC_DOWLEQ:
		if (r->r_dayofmonth == 31) {
			/* be intelligent again */
			fprintf(stdout, "\
;BYDAY=-1%s", wday[r->r_wday]);
		} else {
			fprintf(stdout, "\
;BYDAY=%s;BYMONTHDAY=%d,%d,%d,%d,%d,%d,%d", wday[r->r_wday],
			r->r_dayofmonth - 6,
			r->r_dayofmonth - 5,
			r->r_dayofmonth - 4,
			r->r_dayofmonth - 3,
			r->r_dayofmonth - 2,
			r->r_dayofmonth - 1,
			r->r_dayofmonth - 0);
		}
		break;
	}
	if (r->r_hiyear != ZIC_MAX) {
		fprintf(stdout,
			";UNTIL=%" PRIdZIC "1231T235959Z",
			r->r_hiyear);
	}
	fputc('\n', stdout);

	fputs("END:VEVENT\n", stdout);
	return;
}

static void
pr_ical(const struct zone z[static 1U], size_t nz)
{
	for (size_t i = 0U; i < nz; i++) {
		for (size_t j = 0U; j < z[i].z_nrules; j++) {
			pr_ical_r(z + i, z[i].z_rules + j);
		}
	}
	return;
}


#include "tzicc.yucc"

int
main(int argc, char *argv[])
{
	yuck_t argi[1U];
	int rc = 0;

	if (yuck_parse(argi, argc, argv) < 0) {
		rc = 1;
		goto out;
	}

	/* set up obarrays */
	robs = make_obarray();
	zobs = make_obarray();

	for (size_t i = 0U; i < argi->nargs; i++) {
		const char *fn = argi->args[i];

		zic_fn(fn);
	}

	/* associate rules and zones */
	associate();

	if (LIKELY(nzones)) {
		pr_ical_hdr();
	}
	for (size_t i = 0U, j; i < nzones; i = j) {
		/* find zone span */
		obint_t z = zones[i].z_name;
		const char *zn = obint_name(zobs, z);
		for (j = i + 1U; j < nzones && zones[j].z_name == z; j++);

		/* filter */
		for (size_t k = 0U; k < argi->zone_nargs; k++) {
			if (!strcmp(zn, argi->zone_args[k])) {
				goto pr;
			}
		}
		if (argi->zone_nargs) {
			continue;
		}
	pr:
		pr_ical(zones + i, j - i);
	}
	if (LIKELY(nzones)) {
		pr_ical_ftr();
	}

	/* set them obarrays free again */
	free_obarray(robs);
	free_obarray(zobs);
	free_obarray(NULL);
out:
	yuck_free(argi);
	return rc;
}

/* tzicc.c ends here */
