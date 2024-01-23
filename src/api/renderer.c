#include <string.h>
#include "api.h"
#include "../renderer.h"
#include "../rencache.h"

// a reference index to a table that stores the fonts
static int RENDERER_FONT_REF = LUA_NOREF;

static SCM renfont_type;

static int font_get_options(
  ERenFontAntialiasing *antialiasing,
  ERenFontHinting *hinting,
  int *style
) {
  SCM none = scm_from_utf8_symbol("none");
  SCM grayscale = scm_from_utf8_symbol("none");
  SCM subpixel = scm_from_utf8_symbol("none");
  SCM slight = scm_from_utf8_symbol("none");
  SCM full = scm_from_utf8_symbol("none");

  if (lua_gettop(L) > 2 && lua_istable(L, 3)) {
    lua_getfield(L, 3, "antialiasing");
    if (lua_isstring(L, -1)) {
      const char *antialiasing_str = lua_tostring(L, -1);
      if (antialiasing_str) {
        if (strcmp(antialiasing_str, "none") == 0) {
          *antialiasing = FONT_ANTIALIASING_NONE;
        } else if (strcmp(antialiasing_str, "grayscale") == 0) {
          *antialiasing = FONT_ANTIALIASING_GRAYSCALE;
        } else if (strcmp(antialiasing_str, "subpixel") == 0) {
          *antialiasing = FONT_ANTIALIASING_SUBPIXEL;
        } else {
          return luaL_error(
            L,
            "error in font options, unknown antialiasing option: \"%s\"",
            antialiasing_str
          );
        }
      }
    }
    lua_getfield(L, 3, "hinting");
    if (lua_isstring(L, -1)) {
      const char *hinting_str = lua_tostring(L, -1);
      if (hinting_str) {
        if (strcmp(hinting_str, "slight") == 0) {
          *hinting = FONT_HINTING_SLIGHT;
        } else if (strcmp(hinting_str, "none") == 0) {
          *hinting = FONT_HINTING_NONE;
        } else if (strcmp(hinting_str, "full") == 0) {
          *hinting = FONT_HINTING_FULL;
        } else {
          return luaL_error(
            L,
            "error in font options, unknown hinting option: \"%s\"",
            hinting
          );
        }
      }
    }
    int style_local = 0;
    lua_getfield(L, 3, "italic");
    if (lua_toboolean(L, -1))
      style_local |= FONT_STYLE_ITALIC;
    lua_getfield(L, 3, "bold");
    if (lua_toboolean(L, -1))
      style_local |= FONT_STYLE_BOLD;
    lua_getfield(L, 3, "underline");
    if (lua_toboolean(L, -1))
      style_local |= FONT_STYLE_UNDERLINE;
    lua_getfield(L, 3, "smoothing");
    if (lua_toboolean(L, -1))
      style_local |= FONT_STYLE_SMOOTH;
    lua_getfield(L, 3, "strikethrough");
    if (lua_toboolean(L, -1))
      style_local |= FONT_STYLE_STRIKETHROUGH;

    lua_pop(L, 5);

    if (style_local != 0)
      *style = style_local;
  }

  return 0;
}

static SCM f_font_load(SCM filename, SCM size) {
  const char *filename  = scm_to_utf8_stringn(filename, NULL);
  float size = scm_to_float(size);
  int style = 0;
  ERenFontHinting hinting = FONT_HINTING_SLIGHT;
  ERenFontAntialiasing antialiasing = FONT_ANTIALIASING_SUBPIXEL;

  int ret_code = font_get_options(L, &antialiasing, &hinting, &style);
  if (ret_code > 0)
    return ret_code;

  RenFont** font = scm_gc_malloc(sizeof(RenFont*), "RenFont")
  *font = ren_font_load(window_renderer, filename, size, antialiasing, hinting, style);
  if (!*font)
  {
    return scm_throw("failed to load font");
  }

  return scm_make_foreign_object_1(renfont_type, font);
}

static bool font_retrieve(RenFont** fonts) {
  memset(fonts, 0, sizeof(RenFont*)*FONT_FALLBACK_MAX);

  if (lua_type(L, idx) != LUA_TTABLE) {
    fonts[0] = *(RenFont**)luaL_checkudata(L, idx, API_TYPE_FONT);
    return false;
  }
  int len = luaL_len(L, idx); len = len > FONT_FALLBACK_MAX ? FONT_FALLBACK_MAX : len;
  for (int i = 0; i < len; i++) {
    lua_rawgeti(L, idx, i+1);
    fonts[i] = *(RenFont**) luaL_checkudata(L, -1, API_TYPE_FONT);
    lua_pop(L, 1);
  }
  return true;
}

static SCM f_font_copy() {
  RenFont* fonts[FONT_FALLBACK_MAX];
  bool table = font_retrieve(L, fonts, 1);
  float size = lua_gettop(L) >= 2 ? luaL_checknumber(L, 2) : ren_font_group_get_height(fonts);
  int style = -1;
  ERenFontHinting hinting = -1;
  ERenFontAntialiasing antialiasing = -1;

  int ret_code = font_get_options(L, &antialiasing, &hinting, &style);
  if (ret_code > 0)
    return ret_code;

  if (table) {
    lua_newtable(L);
    luaL_setmetatable(L, API_TYPE_FONT);
  }
  for (int i = 0; i < FONT_FALLBACK_MAX && fonts[i]; ++i) {
    RenFont** font = lua_newuserdata(L, sizeof(RenFont*));
    *font = ren_font_copy(window_renderer, fonts[i], size, antialiasing, hinting, style);
    if (!*font)
      return luaL_error(L, "failed to copy font");
    luaL_setmetatable(L, API_TYPE_FONT);
    if (table)
      lua_rawseti(L, -2, i+1);
  }
  return 1;
}

static SCM f_font_group() {
  int table_size;
  luaL_checktype(L, 1, LUA_TTABLE);

  table_size = lua_rawlen(L, 1);
  if (table_size <= 0)
    return luaL_error(L, "failed to create font group: table is empty");
  if (table_size > FONT_FALLBACK_MAX)
    return luaL_error(L, "failed to create font group: table size too large");

  // we also need to ensure that there are no fontgroups inside it
  for (int i = 1; i <= table_size; i++) {
    if (lua_rawgeti(L, 1, i) != LUA_TUSERDATA)
      return luaL_typeerror(L, -1, API_TYPE_FONT "(userdata)");
    lua_pop(L, 1);
  }

  luaL_setmetatable(L, API_TYPE_FONT);
  return 1;
}

static SCM f_font_get_path(SCM font) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  bool table = font_retrieve(L, fonts, 1);

  if (table) {
    lua_newtable(L);
  }
  for (int i = 0; i < FONT_FALLBACK_MAX && fonts[i]; ++i) {
    const char* path = ren_font_get_path(fonts[i]);
    lua_pushstring(L, path);
    if (table)
      lua_rawseti(L, -2, i+1);
  }
  return 1;
}

static SCM f_font_set_tab_size(SCM font, SCM size) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(L, fonts, 1);
  int n = scm_to_int(size);
  ren_font_group_set_tab_size(fonts, n);
  return 0;
}

//static int f_font_gc() {
//  if (lua_istable(L, 1)) return 0; // do not run if its FontGroup
//  RenFont** self = luaL_checkudata(L, 1, API_TYPE_FONT);
//  ren_font_free(*self);
//
//  return 0;
//}

static SCM f_font_get_width(SCM font, SCM string) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(fonts);
  size_t len;
  const char *text = scm_to_utf8_stringn(string, &len);

  return scm_from_int(ren_font_group_get_width(window_renderer, fonts, text, len, NULL));
}

static SCM f_font_get_height(SCM font) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(fonts);
  return scm_from_int(ren_font_group_get_height(fonts));
}

static SCM f_font_get_size(SCM font) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(fonts);
  return scm_from_float(ren_font_group_get_size(fonts));
}

static SCM f_font_set_size(SCM size) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(fonts);
  float size = scm_to_float(size);
  ren_font_group_set_size(window_renderer, fonts, size);
  return SCM_UNSPECIFIED;
}


static RenColor checkcolor(lua_State *L, int idx, int def) {
  RenColor color;
  if (lua_isnoneornil(L, idx)) {
    return (RenColor) { def, def, def, 255 };
  }
  luaL_checktype(L, idx, LUA_TTABLE);
  color.r = get_color_value(L, idx, 1);
  color.g = get_color_value(L, idx, 2);
  color.b = get_color_value(L, idx, 3);
  color.a = get_color_value_opt(L, idx, 4, 255);
  lua_pop(L, 4);
  return color;
}


static SCM f_show_debug(SCM boolean) {
  rencache_show_debug(scm_is_true(boolean));
  return SCM_UNSPECIFIED;
}


static SCM f_get_size() {
  int w, h;
  ren_get_size(window_renderer, &w, &h);
  lua_pushnumber(L, w);
  lua_pushnumber(L, h);
  return 2;
}


static SCM f_begin_frame() {
  rencache_begin_frame(window_renderer);
  return 0;
}


static SCM f_end_frame() {
  rencache_end_frame(window_renderer);
  // clear the font reference table
  lua_newtable(L);
  lua_rawseti(L, LUA_REGISTRYINDEX, RENDERER_FONT_REF);
  return SCM_UNSPECIFIED;
}


static RenRect rect_to_grid(int x, int y, int w, int h) {
  int x1 = (int) (x + 0.5), y1 = (int) (y + 0.5);
  int x2 = (int) (x + w + 0.5), y2 = (int) (y + h + 0.5);
  return (RenRect) {x1, y1, x2 - x1, y2 - y1};
}


static SCM f_set_clip_rect(SCM x, SCM y, SCM w, SCM h) {
  RenRect rect = rect_to_grid(scm_to_int(x), scm_to_int(y), scm_to_int(w), scm_to_int(h));
  rencache_set_clip_rect(window_renderer, rect);
  return SCM_UNSPECIFIED;
}


static SCM f_draw_rect(SCM x, SCM y, SCM w, SCM h, SCM color) {
  RenRect rect = rect_to_grid(scm_to_int(x), scm_to_int(y), scm_to_int(w), scm_to_int(h));
  RenColor color = checkcolor(color);
  rencache_draw_rect(window_renderer, rect, color);
  return 0;
}

static SCM f_draw_text(SCM font, SCM text, SCM x, SCM y, SCM color) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(L, fonts, 1);

  size_t len;
  const char *text = scm_to_utf8_stringn(text, NULL);
  double x = scm_to_double(x);
  int y = scm_to_double(y);
  RenColor ren_color = to_color(color);
  x = rencache_draw_text(window_renderer, fonts, text, len, x, y, ren_color);
  return scm_from_double(x);
}

//static const luaL_Reg lib[] = {
//  { "show_debug",         f_show_debug         },
//  { "get_size",           f_get_size           },
//  { "begin_frame",        f_begin_frame        },
//  { "end_frame",          f_end_frame          },
//  { "set_clip_rect",      f_set_clip_rect      },
//  { "draw_rect",          f_draw_rect          },
//  { "draw_text",          f_draw_text          },
//  { NULL,                 NULL                 }
//};
//
//static const luaL_Reg fontLib[] = {
//  { "__gc",               f_font_gc                 },
//  { "load",               f_font_load               },
//  { "copy",               f_font_copy               },
//  { "group",              f_font_group              },
//  { "set_tab_size",       f_font_set_tab_size       },
//  { "get_width",          f_font_get_width          },
//  { "get_height",         f_font_get_height         },
//  { "get_size",           f_font_get_size           },
//  { "set_size",           f_font_set_size           },
//  { "get_path",           f_font_get_path           },
//};
//
//void register_font_lib() {
//}
