#include <SDL.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "api.h"
#include "../rencache.h"
#include "../renwindow.h"
#ifdef _WIN32
  #include <direct.h>
  #include <windows.h>
  #include <fileapi.h>
  #include "../utfconv.h"

  // Windows does not define the S_ISREG and S_ISDIR macros in stat.h, so we do.
  // We have to define _CRT_INTERNAL_NONSTDC_NAMES 1 before #including sys/stat.h
  // in order for Microsoft's stat.h to define names like S_IFMT, S_IFREG, and S_IFDIR,
  // rather than just defining  _S_IFMT, _S_IFREG, and _S_IFDIR as it normally does.
  #define _CRT_INTERNAL_NONSTDC_NAMES 1
  #include <sys/types.h>
  #include <sys/stat.h>
  #if !defined(S_ISREG) && defined(S_IFMT) && defined(S_IFREG)
    #define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
  #endif
  #if !defined(S_ISDIR) && defined(S_IFMT) && defined(S_IFDIR)
    #define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
  #endif
#else

#include <dirent.h>
#include <unistd.h>

#ifdef __linux__
  #include <sys/vfs.h>
#endif
#endif

static const char* button_name(int button) {
  switch (button) {
    case SDL_BUTTON_LEFT   : return "left";
    case SDL_BUTTON_MIDDLE : return "middle";
    case SDL_BUTTON_RIGHT  : return "right";
    case SDL_BUTTON_X1     : return "x";
    case SDL_BUTTON_X2     : return "y";
    default : return "?";
  }
}


static void str_tolower(char *p) {
  while (*p) {
    *p = tolower(*p);
    p++;
  }
}

struct HitTestInfo {
  int title_height;
  int controls_width;
  int resize_border;
};
typedef struct HitTestInfo HitTestInfo;

static HitTestInfo window_hit_info[1] = {{0, 0, 0}};

#define RESIZE_FROM_TOP 0
#define RESIZE_FROM_RIGHT 0

static SCM sym_type,
           sym_quit,
           sym_resized,
           sym_minimized,
           sym_maximized,
           sym_normal,
           sym_fullscreen,
           sym_restored,
           sym_exposed,
           sym_mouse_left,
           sym_key_released,
           sym_key_pressed,
           sym_focus_lost,
           sym_text_input,
           sym_event,
           sym_width,
           sym_height,
           sym_normal,
           sym_key,
           sym_x,
           sym_y,
  sym_mouse_wheel,
           sym_text;

static SDL_HitTestResult SDLCALL hit_test(SDL_Window *window, const SDL_Point *pt, void *data) {
  const HitTestInfo *hit_info = (HitTestInfo *) data;
  const int resize_border = hit_info->resize_border;
  const int controls_width = hit_info->controls_width;
  int w, h;

  SDL_GetWindowSize(window_renderer->window, &w, &h);

  if (pt->y < hit_info->title_height &&
    #if RESIZE_FROM_TOP
    pt->y > hit_info->resize_border &&
    #endif
    pt->x > resize_border && pt->x < w - controls_width) {
    return SDL_HITTEST_DRAGGABLE;
  }

  #define REPORT_RESIZE_HIT(name) { \
    return SDL_HITTEST_RESIZE_##name; \
  }

  if (pt->x < resize_border && pt->y < resize_border) {
    REPORT_RESIZE_HIT(TOPLEFT);
  #if RESIZE_FROM_TOP
  } else if (pt->x > resize_border && pt->x < w - controls_width && pt->y < resize_border) {
    REPORT_RESIZE_HIT(TOP);
  #endif
  } else if (pt->x > w - resize_border && pt->y < resize_border) {
    REPORT_RESIZE_HIT(TOPRIGHT);
  #if RESIZE_FROM_RIGHT
  } else if (pt->x > w - resize_border && pt->y > resize_border && pt->y < h - resize_border) {
    REPORT_RESIZE_HIT(RIGHT);
  #endif
  } else if (pt->x > w - resize_border && pt->y > h - resize_border) {
    REPORT_RESIZE_HIT(BOTTOMRIGHT);
  } else if (pt->x < w - resize_border && pt->x > resize_border && pt->y > h - resize_border) {
    REPORT_RESIZE_HIT(BOTTOM);
  } else if (pt->x < resize_border && pt->y > h - resize_border) {
    REPORT_RESIZE_HIT(BOTTOMLEFT);
  } else if (pt->x < resize_border && pt->y < h - resize_border && pt->y > resize_border) {
    REPORT_RESIZE_HIT(LEFT);
  }

  return SDL_HITTEST_NORMAL;
}

static const char *numpad[] = { "end", "down", "pagedown", "left", "", "right", "home", "up", "pageup", "ins", "delete" };

static const char *get_key_name(const SDL_Event *e, char *buf) {
  SDL_Scancode scancode = e->key.keysym.scancode;
  /* Is the scancode from the keypad and the number-lock off?
  ** We assume that SDL_SCANCODE_KP_1 up to SDL_SCANCODE_KP_9 and SDL_SCANCODE_KP_0
  ** and SDL_SCANCODE_KP_PERIOD are declared in SDL2 in that order. */
  if (scancode >= SDL_SCANCODE_KP_1 && scancode <= SDL_SCANCODE_KP_1 + 10 &&
    !(e->key.keysym.mod & KMOD_NUM)) {
    return numpad[scancode - SDL_SCANCODE_KP_1];
  } else {
    /* We need to correctly handle non-standard layouts such as dvorak.
       Therefore, if a Latin letter(code<128) is pressed in the current layout,
       then we transmit it as it is. But we also need to support shortcuts in
       other languages, so for non-Latin characters(code>128) we pass the
       scancode based name that matches the letter in the QWERTY layout.

       In SDL, the codes of all special buttons such as control, shift, arrows
       and others, are masked with SDLK_SCANCODE_MASK, which moves them outside
       the unicode range (>0x10FFFF). Users can remap these buttons, so we need
       to return the correct name, not scancode based. */
    if ((e->key.keysym.sym < 128) || (e->key.keysym.sym & SDLK_SCANCODE_MASK))
      strcpy(buf, SDL_GetKeyName(e->key.keysym.sym));
    else
      strcpy(buf, SDL_GetScancodeName(scancode));
    str_tolower(buf);
    return buf;
  }
}

#ifdef _WIN32
static char *win32_error(DWORD rc) {
  LPSTR message;
  FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    rc,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPTSTR) &message,
    0,
    NULL
  );

  return message;
}

#endif

static SCM make_alist(SCM keyvals[]) {
  SCM alist = scm_list_n(SCM_UNDEFINED);
  for (int i = 0; keyvals[i] != SCM_UNDEFINED; i += 2) {
    alist = scm_acons(keyvals[i], keyvals[i+1], alist);
  }
  return alist;
}

static SCM f_poll_event() {
  char buf[16];
  int mx, my, w, h;
  SDL_Event e;
  SDL_Event event_plus;

top:
  if ( !SDL_PollEvent(&e) ) {
    return SCM_BOOL_F;
  }

  switch (e.type) {
    case SDL_QUIT:
      return make_alist( (SCM[]) {
        sym_type, sym_quit,
        SCM_UNDEFINED
      });

  case SDL_MOUSEWHEEL: {
    SDL_MouseWheelEvent wheel = e.wheel;
    return make_alist( (SCM[]) {
	sym_type, sym_mouse_wheel,
	sym_x, scm_from_double(wheel.preciseX),
	sym_y, scm_from_double(wheel.preciseY),
	SCM_UNDEFINED
      });
  } break;

    case SDL_WINDOWEVENT:
      if (e.window.event == SDL_WINDOWEVENT_RESIZED) {
        ren_resize_window(window_renderer);
        return make_alist( (SCM[]) {
          sym_type, sym_resized,
          sym_width, scm_from_int(e.window.data1),
          sym_height, scm_from_int(e.window.data2),
          SCM_UNDEFINED
        });
        break;
      } else if (e.window.event == SDL_WINDOWEVENT_EXPOSED) {
        rencache_invalidate();
        return make_alist( (SCM[]) {
          sym_type, sym_exposed,
          SCM_UNDEFINED
        });
      } else if (e.window.event == SDL_WINDOWEVENT_MINIMIZED) {
        return make_alist( (SCM[]) {
          sym_type, sym_minimized,
          SCM_UNDEFINED
        });
      } else if (e.window.event == SDL_WINDOWEVENT_MAXIMIZED) {
        return make_alist( (SCM[]) {
          sym_type, sym_maximized,
          SCM_UNDEFINED
        });
      } else if (e.window.event == SDL_WINDOWEVENT_RESTORED) {
        return make_alist( (SCM[]) {
          sym_type, sym_restored,
          SCM_UNDEFINED
        });
      } else if (e.window.event == SDL_WINDOWEVENT_LEAVE) {
        return make_alist( (SCM[]) {
          sym_type, sym_mouse_left,
          SCM_UNDEFINED
        });
      }
      if (e.window.event == SDL_WINDOWEVENT_FOCUS_LOST) {
        return make_alist( (SCM[]) {
          sym_type, sym_focus_lost,
          SCM_UNDEFINED
        });
      }
      /* on some systems, when alt-tabbing to the window SDL will queue up
      ** several KEYDOWN events for the `tab` key; we flush all keydown
      ** events on focus so these are discarded */
      if (e.window.event == SDL_WINDOWEVENT_FOCUS_GAINED) {
        SDL_FlushEvent(SDL_KEYDOWN);
      }
      goto top;

    case SDL_KEYDOWN:
#ifdef __APPLE__
      /* on macos 11.2.3 with sdl 2.0.14 the keyup handler for cmd+w below
      ** was not enough. Maybe the quit event started to be triggered from the
      ** keydown handler? In any case, flushing the quit event here too helped. */
      if ((e.key.keysym.sym == SDLK_w) && (e.key.keysym.mod & KMOD_GUI)) {
        SDL_FlushEvent(SDL_QUIT);
      }
#endif
      return make_alist( (SCM[]) {
        sym_type, sym_key_pressed,
        sym_key, scm_from_utf8_string(get_key_name(&e, buf)),
        SCM_UNDEFINED
      });
    case SDL_KEYUP:
#ifdef __APPLE__
      /* on macos command+w will close the current window
      ** we want to flush this event and let the keymapper
      ** handle this key combination.
      ** Thanks to mathewmariani, taken from his lite-macos github repository. */
      if ((e.key.keysym.sym == SDLK_w) && (e.key.keysym.mod & KMOD_GUI)) {
        SDL_FlushEvent(SDL_QUIT);
      }
#endif
      return make_alist( (SCM[]) {
        sym_type, sym_key_released,
        sym_key, scm_from_utf8_string(get_key_name(&e, buf)),
        SCM_UNDEFINED
      });
    case SDL_TEXTINPUT:
      return make_alist( (SCM[]) {
        sym_type, sym_text_input,
        sym_text, scm_from_utf8_string(e.text.text),
        SCM_UNDEFINED
      });
    case SDL_APP_WILLENTERFOREGROUND:
    case SDL_APP_DIDENTERFOREGROUND:
      #ifdef LITE_USE_SDL_RENDERER
        rencache_invalidate();
      #else
        SDL_UpdateWindowSurface(window_renderer->window);
      #endif
    default:
      goto top;
  }

  return SCM_BOOL_F;
}



static SDL_Cursor* cursor_cache[SDL_SYSTEM_CURSOR_HAND + 1];

static const char *cursor_opts[] = {
  "arrow",
  "ibeam",
  "sizeh",
  "sizev",
  "hand",
  NULL
};

static const int cursor_enums[] = {
  SDL_SYSTEM_CURSOR_ARROW,
  SDL_SYSTEM_CURSOR_IBEAM,
  SDL_SYSTEM_CURSOR_SIZEWE,
  SDL_SYSTEM_CURSOR_SIZENS,
  SDL_SYSTEM_CURSOR_HAND
};

enum { WIN_NORMAL, WIN_MINIMIZED, WIN_MAXIMIZED, WIN_FULLSCREEN };

static SCM f_set_window_mode(SCM mode) {
  int n;
  if (scm_is_true(scm_equal_p(mode, sym_normal))) {
    n = WIN_NORMAL;
  } else if (scm_is_true(scm_equal_p(mode, sym_minimized))) {
    n = WIN_MINIMIZED;
  } else if (scm_is_true(scm_equal_p(mode, sym_maximized))) {
    n = WIN_MAXIMIZED;
  } else if (scm_is_true(scm_equal_p(mode, sym_fullscreen))) {
    n = WIN_FULLSCREEN;
  } else {
    return SCM_BOOL_F;
  }

  SDL_SetWindowFullscreen(window_renderer->window,
    n == WIN_FULLSCREEN ? SDL_WINDOW_FULLSCREEN_DESKTOP : 0);
  if (n == WIN_NORMAL) { SDL_RestoreWindow(window_renderer->window); }
  if (n == WIN_MAXIMIZED) { SDL_MaximizeWindow(window_renderer->window); }
  if (n == WIN_MINIMIZED) { SDL_MinimizeWindow(window_renderer->window); }

  return SCM_BOOL_T;
}



static SCM f_get_window_size() {
  int x, y, w, h;
  SDL_GetWindowSize(window_renderer->window, &w, &h);
  SDL_GetWindowPosition(window_renderer->window, &x, &y);
  return make_alist((SCM[]) {
    sym_width, scm_from_int(w),
    sym_height, scm_from_int(h),
    sym_x, scm_from_int(x),
    sym_y, scm_from_int(y),
    SCM_UNDEFINED
  });
}


static SCM f_set_window_size(SCM w_x, SCM w_y, SCM w_w, SCM w_h) {
  double w = scm_to_double(w_w);
  double h = scm_to_double(w_h);
  double x = scm_to_double(w_x);
  double y = scm_to_double(w_y);
  SDL_SetWindowSize(window_renderer->window, w, h);
  SDL_SetWindowPosition(window_renderer->window, x, y);
  ren_resize_window(window_renderer);
  return SCM_UNSPECIFIED;
}


void api_load_system() {
  sym_type = scm_from_utf8_symbol("type");
  sym_quit = scm_from_utf8_symbol("quit");
  sym_resized = scm_from_utf8_symbol("resized");
  sym_minimized = scm_from_utf8_symbol("minimized");
  sym_maximized = scm_from_utf8_symbol("maximized");
  sym_normal = scm_from_utf8_symbol("normal");
  sym_fullscreen = scm_from_utf8_symbol("fullscreen");
  sym_restored = scm_from_utf8_symbol("restored");
  sym_exposed = scm_from_utf8_symbol("exposed");
  sym_mouse_left = scm_from_utf8_symbol("mouse-left");
  sym_key_released = scm_from_utf8_symbol("key-released");
  sym_key_pressed = scm_from_utf8_symbol("key-pressed");
  sym_focus_lost = scm_from_utf8_symbol("focus-lost");
  sym_text_input = scm_from_utf8_symbol("text-input");
  sym_mouse_wheel = scm_from_utf8_symbol("mouse-wheel");
  sym_event = scm_from_utf8_symbol("event");
  sym_width = scm_from_utf8_symbol("width");
  sym_height = scm_from_utf8_symbol("height");
  sym_normal = scm_from_utf8_symbol("normal");
  sym_key = scm_from_utf8_symbol("key");
  sym_text = scm_from_utf8_symbol("text");
  sym_x = scm_from_utf8_symbol("x");
  sym_y = scm_from_utf8_symbol("y");

  scm_c_define_gsubr("poll-event", 0, 0, 0, f_poll_event);
  scm_c_define_gsubr("set-window-mode", 1, 0, 0, f_set_window_mode);
  scm_c_define_gsubr("set-window-size", 4, 0, 0, f_set_window_size);
  scm_c_define_gsubr("get-window-size", 0, 0, 0, f_get_window_size);
}
