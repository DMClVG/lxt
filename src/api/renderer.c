#include <string.h>
#include "api.h"
#include "../renderer.h"
#include "../rencache.h"

static SCM renfont_type;

SCM f_font_load(SCM font_filename, SCM font_size) {
  const char *filename  = scm_to_utf8_stringn(font_filename, NULL);
  float size = (float) scm_to_double(font_size);
  int style = 0;
  ERenFontHinting hinting = FONT_HINTING_SLIGHT;
  ERenFontAntialiasing antialiasing = FONT_ANTIALIASING_SUBPIXEL;

  RenFont** font = (RenFont**) scm_gc_malloc(sizeof(RenFont*), "RenFont");
  *font = ren_font_load(window_renderer, filename, size, antialiasing, hinting, style);
  if (!*font) {
    scm_throw(scm_from_utf8_symbol("load-font-failed"), scm_list_n(SCM_UNDEFINED));
    return SCM_UNSPECIFIED;
  }

  return scm_make_foreign_object_1(renfont_type, font);
}

static RenRect rect_to_grid(int x, int y, int w, int h) {
  int x1 = (int) (x + 0.5), y1 = (int) (y + 0.5);
  int x2 = (int) (x + w + 0.5), y2 = (int) (y + h + 0.5);
  return (RenRect) {x1, y1, x2 - x1, y2 - y1};
}

static RenColor to_color(SCM object) {
  RenColor color;
  color.r = scm_to_uint8(scm_list_ref(object, scm_from_int(0)));
  color.g = scm_to_uint8(scm_list_ref(object, scm_from_int(1)));
  color.b = scm_to_uint8(scm_list_ref(object, scm_from_int(2)));
  color.a = scm_to_uint8(scm_list_ref(object, scm_from_int(3)));
  return color;
}

static SCM f_draw_rect(SCM rect_x, SCM rect_y, SCM rect_w, SCM rect_h, SCM rect_color) {
  int x = scm_to_int(rect_x);
  int y = scm_to_int(rect_y);
  int w = scm_to_int(rect_w);
  int h = scm_to_int(rect_h);

  RenRect rect = rect_to_grid(x, y, w, h);
  RenColor color = to_color(rect_color);
  rencache_draw_rect(window_renderer, rect, color);
  return SCM_UNSPECIFIED;
}

static bool font_retrieve(RenFont** fonts, SCM font) {
  memset(fonts, 0, sizeof(RenFont*)*FONT_FALLBACK_MAX);
  fonts[0] = *(RenFont**) scm_foreign_object_ref(font, 0);
  return false;
}

static SCM f_draw_text(SCM t_font, SCM t_text, SCM t_x, SCM t_y, SCM t_color) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(fonts, t_font);

  size_t len;
  const char *text = scm_to_utf8_stringn(t_text, &len);
  double x = scm_to_double(t_x);
  int y = scm_to_int(t_y);
  RenColor color = to_color(t_color);
  x = rencache_draw_text(window_renderer, fonts, text, len, x, y, color);
  return scm_from_double(0);
}

static int strlen_utf32(const uint32_t *str, int max)
{
  int len = 0;
  while(str[len])
  {
    if (len + 1 >= max)
      break;
    len++;
  }
  return len;
}

static SCM f_draw_buffer(SCM t_font, SCM t_buffer, SCM t_red, SCM t_green, SCM t_blue, SCM t_w, SCM t_h, SCM t_x, SCM t_y) {
  RenFont* fonts[FONT_FALLBACK_MAX];
  font_retrieve(fonts, t_font);
  int font_height = ren_font_group_get_height(fonts);

  uint32_t *text = (uint32_t*) SCM_BYTEVECTOR_CONTENTS(t_buffer);
  char *red = (char*) SCM_BYTEVECTOR_CONTENTS(t_red);
  char *green = (char*) SCM_BYTEVECTOR_CONTENTS(t_green);
  char *blue = (char*) SCM_BYTEVECTOR_CONTENTS(t_blue);

  double x = scm_to_double(t_x);
  int y = scm_to_int(t_y);
  int w = scm_to_int(t_w);
  int h = scm_to_int(t_h);

  for(int j = 0; j < h; j++)
  {
    uint32_t *row = &text[j * w];
    char *row_red = &red[j * w];
    char *row_green = &green[j * w];
    char *row_blue = &blue[j * w];

//    size_t len = strlen_utf32(row, w);
    size_t len = (size_t) w;
    rencache_draw_buffer(window_renderer, fonts, row, row_red, row_green, row_blue, len, x, y + j * font_height);
  }
  return scm_from_double(0);
}

static SCM f_end_frame() {
  rencache_end_frame(window_renderer);
  // clear the font reference table
  //lua_newtable(L);
  //lua_rawseti(L, LUA_REGISTRYINDEX, RENDERER_FONT_REF); TODO
  return SCM_UNSPECIFIED;
}

static SCM f_begin_frame() {
  rencache_begin_frame(window_renderer);
  return SCM_UNSPECIFIED;
}

void api_load_renderer() {
  renfont_type =
    scm_make_foreign_object_type(
      scm_from_utf8_symbol("font"),
      scm_list_1(scm_from_utf8_symbol("renfont")),
      NULL);

  scm_c_define_gsubr("load-font", 2, 0, 0, f_font_load);
  scm_c_define_gsubr("draw-text", 5, 0, 0, f_draw_text);
  scm_c_define_gsubr("draw-buffer", 9, 0, 0, f_draw_buffer);
  scm_c_define_gsubr("draw-rect", 5, 0, 0, f_draw_rect);
  scm_c_define_gsubr("end-frame", 0, 0, 0, f_end_frame);
  scm_c_define_gsubr("begin-frame", 0, 0, 0, f_begin_frame);
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
