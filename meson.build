project('lite-xl',
    ['c'],
    version : '2.1.2',
    license : 'MIT',
    meson_version : '>= 0.56',
    default_options : [
        'c_std=gnu11'
    ]
)

#===============================================================================
# Project version including git commit if possible
#===============================================================================
version = meson.project_version()

if get_option('buildtype') != 'release'
    git_command = find_program('git', required : false)

    if git_command.found()
        git_commit = run_command(
            [git_command, 'rev-parse', 'HEAD'],
            check : false
        ).stdout().strip()

        if git_commit != ''
            version += ' (git-' + git_commit.substring(0, 8) + ')'
        endif
    endif
endif

#===============================================================================
# Configuration
#===============================================================================
conf_data = configuration_data()
conf_data.set('PROJECT_BUILD_DIR', meson.current_build_dir())
conf_data.set('PROJECT_SOURCE_DIR', meson.current_source_dir())
conf_data.set('PROJECT_VERSION', version)
conf_data.set('PROJECT_ASSEMBLY_VERSION', meson.project_version() + '.0')

#===============================================================================
# Compiler Settings
#===============================================================================
if host_machine.system() == 'darwin'
    add_languages('objc')
endif

cc = meson.get_compiler('c')

lite_includes = []
lite_cargs = ['-DSDL_MAIN_HANDLED', '-DPCRE2_STATIC']
# On macos we need to use the SDL renderer to support retina displays
if get_option('renderer') or host_machine.system() == 'darwin'
    lite_cargs += '-DLITE_USE_SDL_RENDERER'
endif
if get_option('arch_tuple') != ''
    arch_tuple = get_option('arch_tuple')
else
    arch_tuple = '@0@-@1@'.format(target_machine.cpu_family(), target_machine.system())
endif
lite_cargs += '-DLITE_ARCH_TUPLE="@0@"'.format(arch_tuple)

#===============================================================================
# Linker Settings
#===============================================================================
lite_link_args = []
if host_machine.system() == 'darwin'
    lite_link_args += ['-framework', 'CoreServices', '-framework', 'Foundation']
endif
#===============================================================================
# Dependencies
#===============================================================================
if not get_option('source-only')
    libm = cc.find_library('m', required : false)
    libdl = cc.find_library('dl', required : false)

    default_fallback_options = ['warning_level=0', 'werror=false']

    # Lua has no official .pc file
    # so distros come up with their own names
    lua_names = [
        'lua5.4', # Debian
        'lua-5.4', # FreeBSD
        'lua',    # Fedora
    ]

    if get_option('use_system_lua')
        foreach lua : lua_names
            last_lua = (lua == lua_names[-1] or get_option('wrap_mode') == 'forcefallback')
            lua_dep = dependency(lua, required : false,
            )
            if lua_dep.found()
                break
            endif

            if last_lua
                # If we could not find lua on the system and fallbacks are disabled
                # try the compiler as a last ditch effort, since Lua has no official
                # pkg-config support.
                lua_dep = cc.find_library('lua', required : true)
            endif
        endforeach
    else
        lua_dep = dependency('', fallback: ['lua', 'lua_dep'], required : true,
            default_options: default_fallback_options + ['default_library=static', 'line_editing=disabled', 'interpreter=false']
        )
    endif

    pcre2_dep = dependency('libpcre2-8', fallback: ['pcre2', 'libpcre2_8'],
        default_options: default_fallback_options + ['default_library=static', 'grep=false', 'test=false']
    )

    freetype_dep = dependency('freetype2', fallback: ['freetype2', 'freetype_dep'],
        default_options: default_fallback_options + ['default_library=static', 'zlib=disabled', 'bzip2=disabled', 'png=disabled', 'harfbuzz=disabled', 'brotli=disabled']
    )


    sdl_options = ['default_library=static']

    # we explicitly need these
    sdl_options += 'use_loadso=enabled'
    sdl_options += 'prefer_dlopen=true'
    sdl_options += 'use_video=enabled'
    sdl_options += 'use_atomic=enabled'
    sdl_options += 'use_threads=enabled'
    sdl_options += 'use_timers=enabled'
    sdl_options += 'with_main=true'
    # investigate if this is truly needed
    # Do not remove before https://github.com/libsdl-org/SDL/issues/5413 is released
    sdl_options += 'use_events=enabled'

    if host_machine.system() == 'darwin' or host_machine.system() == 'windows'
        sdl_options += 'use_video_x11=disabled'
        sdl_options += 'use_video_wayland=disabled'
    else
        sdl_options += 'use_render=enabled'
        sdl_options += 'use_video_x11=auto'
        sdl_options += 'use_video_wayland=auto'
    endif

    # we leave this up to what the host system has except on windows
    if host_machine.system() != 'windows'
        sdl_options += 'use_video_opengl=auto'
        sdl_options += 'use_video_openglesv2=auto'
    else
        sdl_options += 'use_video_opengl=disabled'
        sdl_options += 'use_video_openglesv2=disabled'
    endif

    # we don't need these
    sdl_options += 'test=false'
    sdl_options += 'use_sensor=disabled'
    sdl_options += 'use_haptic=disabled'
    sdl_options += 'use_audio=disabled'
    sdl_options += 'use_cpuinfo=disabled'
    sdl_options += 'use_joystick=disabled'
    sdl_options += 'use_video_vulkan=disabled'
    sdl_options += 'use_video_offscreen=disabled'
    sdl_options += 'use_power=disabled'
    sdl_options += 'system_iconv=disabled'

    sdl_dep = dependency('sdl2', fallback: ['sdl2', 'sdl2_dep'], method: 'pkg-config',
        default_options: default_fallback_options + sdl_options
    )

    if host_machine.system() == 'windows'
        if sdl_dep.type_name() == 'internal'
            sdlmain_dep = dependency('sdl2main', fallback: ['sdl2main_dep'])
        else
            sdlmain_dep = cc.find_library('SDL2main')
        endif
    else
        sdlmain_dep = dependency('', required: false)
        assert(not sdlmain_dep.found(), 'checking if fake dependency has been found')
    endif

    guile_dep = dependency('guile-3.0')

    lite_deps = [lua_dep, sdl_dep, sdlmain_dep, freetype_dep, pcre2_dep, guile_dep, libm, libdl]
endif
#===============================================================================
# Install Configuration
#===============================================================================
if get_option('portable') or host_machine.system() == 'windows'
    lite_bindir = '/'
    lite_docdir = '/doc'
    lite_datadir = '/data'
    configure_file(
        input: 'resources/windows/lite-xl.exe.manifest.in',
        output: 'lite-xl.exe.manifest',
        configuration: conf_data
    )
elif get_option('bundle') and host_machine.system() == 'darwin'
    lite_cargs += '-DMACOS_USE_BUNDLE'
    lite_bindir = 'Contents/MacOS'
    lite_docdir = 'Contents/Resources'
    lite_datadir = 'Contents/Resources'
    install_data('resources/icons/icon.icns', install_dir : 'Contents/Resources')
    configure_file(
        input : 'resources/macos/Info.plist.in',
        output : 'Info.plist',
        configuration : conf_data,
        install : true,
        install_dir : 'Contents'
    )
else
    lite_bindir = 'bin'
    lite_docdir = 'share/doc/lite-xl'
    lite_datadir = 'share/lite-xl'
    if host_machine.system() == 'linux'
        install_data('resources/icons/lite-xl.svg',
            install_dir : 'share/icons/hicolor/scalable/apps'
        )
        install_data('resources/linux/com.lite_xl.LiteXL.desktop',
            install_dir : 'share/applications'
        )
        install_data('resources/linux/com.lite_xl.LiteXL.appdata.xml',
            install_dir : 'share/metainfo'
        )
    endif
endif

install_data('licenses/licenses.md', install_dir : lite_docdir)

install_subdir('data/core' , install_dir : lite_datadir, exclude_files : 'init.scm')
foreach data_module : ['fonts']
    install_subdir(join_paths('data', data_module), install_dir : lite_datadir)
endforeach

configure_file(
    input : 'data/core/init.scm',
    output : 'init.scm',
    configuration : conf_data,
    install_dir : join_paths(lite_datadir, 'core'),
)


configure_file(
    input : 'data/core/buffer.scm',
    output : 'buffer.scm',
    configuration : conf_data,
    install_dir : join_paths(lite_datadir, 'core'),
)

if not get_option('source-only')
    subdir('src')
    subdir('scripts')
endif
