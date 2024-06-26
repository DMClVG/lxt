lite_sources = [
    'api/api.c',
    'api/renderer.c',
    'api/system.c',
    'renderer.c',
    'renwindow.c',
    'rencache.c',
    'main.c',
]

# dirmonitor backend
if get_option('dirmonitor_backend') == ''
    if cc.has_function('inotify_init', prefix : '#include<sys/inotify.h>')
        dirmonitor_backend = 'inotify'
    elif host_machine.system() == 'darwin' and cc.check_header('CoreServices/CoreServices.h')
        dirmonitor_backend = 'fsevents'
    elif cc.has_function('kqueue', prefix : '#include<sys/event.h>')
        dirmonitor_backend = 'kqueue'
    elif dependency('libkqueue', required : false).found()
        dirmonitor_backend = 'kqueue'
    elif host_machine.system() == 'windows'
        dirmonitor_backend = 'win32'
    else
        dirmonitor_backend = 'dummy'
        warning('no suitable backend found, defaulting to dummy backend')
    endif
else
    dirmonitor_backend = get_option('dirmonitor_backend')
endif

message('dirmonitor_backend: @0@'.format(dirmonitor_backend))

if dirmonitor_backend == 'kqueue'
    libkqueue_dep = dependency('libkqueue', required : false)
    if libkqueue_dep.found()
        lite_deps += libkqueue_dep
    endif
endif

lite_sources += [
]


lite_rc = []
if host_machine.system() == 'windows'
    windows = import('windows')
    lite_rc += windows.compile_resources('../resources/icons/icon.rc')
    lite_rc += windows.compile_resources('../resources/windows/manifest.rc')
elif host_machine.system() == 'darwin'
    lite_sources += 'bundle_open.m'
endif

lite_includes += include_directories('.')

executable('lite-xl',
    lite_sources + lite_rc,
    include_directories: lite_includes,
    dependencies: lite_deps,
    c_args: lite_cargs,
    objc_args: lite_cargs,
    link_args: lite_link_args,
    install_dir: lite_bindir,
    install: true,
    win_subsystem: 'windows',
)
