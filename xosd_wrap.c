/*
 *  File: xosd_wrap.c
 *  Created: Sunday, October 10, 2004
 *  Time-stamp: <07/01/2005 02:39:25 Yann Hodique>
 *  Copyright: Yann Hodique
 *  Email: Yann.Hodique@lifl.fr
 */

/************************************************************************
 *                                                                      *
 * This program is free software; you can redistribute it and/or modify *
 * it under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or    *
 * (at your option) any later version.                                  *
 *                                                                      *
 ************************************************************************/

#include <string.h>
#include <libguile.h>
#include <xosd.h>

static scm_t_bits xosd_tag;

#define XOSD(x) ((xosd *) SCM_SMOB_DATA(x))

int xosd_display_percentage(xosd * osd, int line, int per) {
    return xosd_display(osd,line,XOSD_percentage,per);
}

int xosd_display_string(xosd * osd, int line, char * str) {
    return xosd_display(osd,line,XOSD_string,str);

}

int xosd_display_slider(xosd * osd, int line, int slide) {
    return xosd_display(osd,line,XOSD_slider,slide);
}

static SCM _wrap_xosd_set_bar_length(SCM osd, SCM n) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-bar-length!");
    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG2, "xosd-set-bar-length!");

    xosd_set_bar_length(XOSD(osd), scm_to_int(n));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_is_onscreen(SCM osd) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-onscreen?");
    return (xosd_is_onscreen(XOSD(osd)))?SCM_BOOL_T:SCM_BOOL_F;
}

static SCM _wrap_xosd_wait_until_no_display(SCM osd) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-wait-until-nodisplay");

    xosd_wait_until_no_display(XOSD(osd));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_hide(SCM osd) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-hide");

    xosd_hide(XOSD(osd));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_show(SCM osd) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-show");

    xosd_show(XOSD(osd));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_pos(SCM osd, SCM pos) {
    int xosd_pos;

    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-pos!");
    SCM_ASSERT(SCM_SYMBOLP(pos),pos, SCM_ARG2, "xosd-set-pos!");

    if(!strcmp("top",SCM_SYMBOL_CHARS(pos)))
        xosd_pos = XOSD_top;
    else if(!strcmp("bottom",SCM_SYMBOL_CHARS(pos)))
        xosd_pos = XOSD_bottom;
    else if(!strcmp("middle",SCM_SYMBOL_CHARS(pos)))
        xosd_pos = XOSD_middle;
    else /* throw an error? */
        return SCM_BOOL_F;

    xosd_set_pos(XOSD(osd),xosd_pos);

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_align(SCM osd, SCM pos) {
    int xosd_pos;

    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-align!");
    SCM_ASSERT(SCM_SYMBOLP(pos),pos, SCM_ARG2, "xosd-set-align!");

    if(!strcmp("left",SCM_SYMBOL_CHARS(pos)))
        xosd_pos = XOSD_left;
    else if(!strcmp("right",SCM_SYMBOL_CHARS(pos)))
        xosd_pos = XOSD_right;
    else if(!strcmp("center",SCM_SYMBOL_CHARS(pos)))
        xosd_pos = XOSD_center;
    else /* throw an error? */
        return SCM_BOOL_F;

    xosd_set_align(XOSD(osd),xosd_pos);

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_shadow_offset(SCM osd, SCM n) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-shadow-offset!");
    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG2, "xosd-set-shadow-offset!");

    xosd_set_timeout(XOSD(osd), scm_to_int(n));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_outline_offset(SCM osd, SCM n) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-outline-offset!");
    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG2, "xosd-set-outline-offset!");

    xosd_set_timeout(XOSD(osd), scm_to_int(n));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_horizontal_offset(SCM osd, SCM n) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-horizontal-offset!");
    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG2, "xosd-set-horizontal-offset!");

    xosd_set_horizontal_offset(XOSD(osd), scm_to_int(n));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_vertical_offset(SCM osd, SCM n) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-vertival-offset!");
    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG2, "xosd-set-vertical-offset!");

    xosd_set_vertical_offset(XOSD(osd), scm_to_int(n));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_timeout(SCM osd, SCM time) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-timeout!");
    SCM_ASSERT(scm_is_integer(time), time, SCM_ARG2, "xosd-set-timeout!");

    xosd_set_timeout(XOSD(osd), scm_to_int(time));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_scroll(SCM osd, SCM n) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-scroll");
    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG2, "xosd-scroll");

    xosd_scroll(XOSD(osd), scm_to_int(n));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_outline_colour(SCM osd, SCM str) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-outline-colour!");
    SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "xosd-set-outline-coulour!");

    xosd_set_outline_colour(XOSD(osd),SCM_STRING_CHARS(str));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_shadow_colour(SCM osd, SCM str) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-shadow-colour!");
    SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "xosd-set-shadow-coulour!");

    xosd_set_shadow_colour(XOSD(osd),SCM_STRING_CHARS(str));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_colour(SCM osd, SCM str) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-colour!");
    SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "xosd-set-colour!");

    xosd_set_colour(XOSD(osd),SCM_STRING_CHARS(str));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_set_font(SCM osd, SCM str) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-set-font!");
    SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "xosd-set-font!");

    xosd_set_font(XOSD(osd),SCM_STRING_CHARS(str));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_get_colour(SCM osd) {
    int r,g,b;
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-get-colour");

    xosd_get_colour(XOSD(osd),&r,&g,&b);
    return scm_cons(SCM_MAKINUM(r),scm_cons(SCM_MAKINUM(g),scm_cons(SCM_MAKINUM(b), SCM_EOL)));
}

static SCM _wrap_xosd_get_number_lines(SCM osd) {
    int lines;

    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-get-number-lines");
    lines = xosd_get_number_lines(XOSD(osd));
    return SCM_MAKINUM(lines);
}

static SCM _wrap_xosd_display_string(SCM osd, SCM line, SCM str) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-display-string");
    SCM_ASSERT(scm_is_integer(line), line, SCM_ARG2, "xosd-display-string");
    SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG3, "xosd-display-string");

    xosd_display_string(XOSD(osd),scm_to_int(line),SCM_STRING_CHARS(str));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_display_percentage(SCM osd, SCM line, SCM per) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-display-percentage");
    SCM_ASSERT(scm_is_integer(line), line, SCM_ARG2, "xosd-display-percentage");
    SCM_ASSERT(scm_is_integer(per), per, SCM_ARG3, "xosd-display-percentage");

    xosd_display_percentage(XOSD(osd), scm_to_int(line), scm_to_int(per));

    return SCM_UNSPECIFIED;
}

static SCM _wrap_xosd_display_slider(SCM osd, SCM line, SCM per) {
    SCM_ASSERT(SCM_SMOB_PREDICATE(xosd_tag,osd), osd, SCM_ARG1, "xosd-display-slider");
    SCM_ASSERT(scm_is_integer(line), line, SCM_ARG2, "xosd-display-slider");
    SCM_ASSERT(scm_is_integer(per), per, SCM_ARG3, "xosd-display-slider");

    xosd_display_slider(XOSD(osd), scm_to_int(line), scm_to_int(per));

    return SCM_UNSPECIFIED;
}

static SCM make_xosd(SCM n) {
    xosd * w;

    SCM_ASSERT(scm_is_integer(n), n, SCM_ARG1, "make-xosd");
    w = xosd_create(scm_to_int(n));
    SCM_RETURN_NEWSMOB(xosd_tag, w);
}

static SCM mark_xosd(SCM xosd_smob) {
    return SCM_BOOL_F;
}

static size_t free_xosd(SCM xosd_smob) {
    xosd * osd = XOSD(xosd_smob);
    xosd_destroy(osd);
    return sizeof(xosd*);
}

/* Linkage: module */
static void init_helper(void *data) {
    xosd_tag = scm_make_smob_type("xosd",sizeof(xosd*));
    scm_set_smob_mark(xosd_tag,mark_xosd);
    scm_set_smob_free(xosd_tag,free_xosd);

    scm_c_define_gsubr("make-xosd", 1, 0, 0, make_xosd);

    scm_c_define_gsubr("xosd-set-bar-length!", 2, 0, 0, _wrap_xosd_set_bar_length);
    scm_c_define_gsubr("xosd-onscreen?", 1, 0, 0, _wrap_xosd_is_onscreen);
    scm_c_define_gsubr("xosd-wait-until-no-display", 1, 0, 0, _wrap_xosd_wait_until_no_display);
    scm_c_define_gsubr("xosd-hide", 1, 0, 0, _wrap_xosd_hide);
    scm_c_define_gsubr("xosd-show", 1, 0, 0, _wrap_xosd_show);
    scm_c_define_gsubr("xosd-set-pos!", 2, 0, 0, _wrap_xosd_set_pos);
    scm_c_define_gsubr("xosd-set-align!", 2, 0, 0, _wrap_xosd_set_align);
    scm_c_define_gsubr("xosd-set-shadow-offset!", 2, 0, 0, _wrap_xosd_set_shadow_offset);
    scm_c_define_gsubr("xosd-set-outline-offset!", 2, 0, 0, _wrap_xosd_set_outline_offset);
    scm_c_define_gsubr("xosd-set-outline-colour!", 2, 0, 0, _wrap_xosd_set_outline_colour);
    scm_c_define_gsubr("xosd-set-shadow-colour!", 2, 0, 0, _wrap_xosd_set_shadow_colour);
    scm_c_define_gsubr("xosd-set-horizontal-offset!", 2, 0, 0, _wrap_xosd_set_horizontal_offset);
    scm_c_define_gsubr("xosd-set-vertical-offset!", 2, 0, 0, _wrap_xosd_set_vertical_offset);
    scm_c_define_gsubr("xosd-set-timeout!", 2, 0, 0, _wrap_xosd_set_timeout);
    scm_c_define_gsubr("xosd-set-colour!", 2, 0, 0, _wrap_xosd_set_colour);
    scm_c_define_gsubr("xosd-set-font!", 2, 0, 0, _wrap_xosd_set_font);
    scm_c_define_gsubr("xosd-get-colour", 1, 0, 0, _wrap_xosd_get_colour);
    scm_c_define_gsubr("xosd-scroll", 2, 0, 0, _wrap_xosd_scroll);
    scm_c_define_gsubr("xosd-get-number-lines", 1, 0, 0, _wrap_xosd_get_number_lines);
    scm_c_define_gsubr("xosd-display-percentage", 3, 0, 0, _wrap_xosd_display_percentage);
    scm_c_define_gsubr("xosd-display-slider", 3, 0, 0, _wrap_xosd_display_slider);
    scm_c_define_gsubr("xosd-display-string", 3, 0, 0, _wrap_xosd_display_string);

    scm_c_export("make-xosd",
                 "xosd-set-bar-length!",
                 "xosd-onscreen?",
                 "xosd-wait-until-no-display",
                 "xosd-hide",
                 "xosd-show",
                 "xosd-set-pos!",
                 "xosd-set-align!",
                 "xosd-set-shadow-offset!",
                 "xosd-set-outline-offset!",
                 "xosd-set-outline-colour!",
                 "xosd-set-shadow-colour!",
                 "xosd-set-horizontal-offset!",
                 "xosd-set-vertical-offset!",
                 "xosd-set-timeout!",
                 "xosd-set-colour!",
                 "xosd-set-font!",
                 "xosd-get-colour",
                 "xosd-scroll",
                 "xosd-get-number-lines",
                 "xosd-display-percentage",
                 "xosd-display-slider",
                 "xosd-display-string",
                 NULL);
}

SCM
scm_init_xosdguile_module (void) {
    SCM module;
    module = scm_c_define_module("xosdguile", init_helper, NULL);
    return SCM_UNSPECIFIED;
}
