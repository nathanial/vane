/*
 * Vane PTY FFI - Pseudo-terminal management for the terminal emulator
 *
 * Provides forkpty-based shell spawning and I/O for macOS.
 */

#include <lean/lean.h>
#include <util.h>       // forkpty
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

/* PTY handle structure */
typedef struct {
    int master_fd;      /* PTY master file descriptor */
    pid_t child_pid;    /* Shell process PID */
} vane_pty_t;

/* External class for garbage collection */
static lean_external_class *g_pty_class = NULL;

/* Finalizer - clean up PTY when garbage collected */
static void vane_pty_finalizer(void *ptr) {
    vane_pty_t *pty = (vane_pty_t *)ptr;
    if (pty) {
        if (pty->master_fd >= 0) {
            close(pty->master_fd);
        }
        if (pty->child_pid > 0) {
            kill(pty->child_pid, SIGHUP);
            waitpid(pty->child_pid, NULL, WNOHANG);
        }
        free(pty);
    }
}

/* Initialize external class (called on first use) */
static void vane_pty_init_class(void) {
    if (g_pty_class == NULL) {
        g_pty_class = lean_register_external_class(vane_pty_finalizer, NULL);
    }
}

/* Box/unbox PTY handle */
static inline lean_object *vane_pty_box(vane_pty_t *pty) {
    vane_pty_init_class();
    return lean_alloc_external(g_pty_class, pty);
}

static inline vane_pty_t *vane_pty_unbox(lean_object *obj) {
    return (vane_pty_t *)lean_get_external_data(obj);
}

/*
 * Open a new PTY with shell
 *
 * @param shell Shell path (e.g., "/bin/zsh")
 * @param cols Terminal width in columns
 * @param rows Terminal height in rows
 * @return PTY handle or IO error
 */
LEAN_EXPORT lean_obj_res vane_pty_open(
    b_lean_obj_arg shell,
    uint16_t cols,
    uint16_t rows,
    lean_obj_arg world
) {
    vane_pty_t *pty = (vane_pty_t *)malloc(sizeof(vane_pty_t));
    if (!pty) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate PTY structure")));
    }

    struct winsize ws;
    memset(&ws, 0, sizeof(ws));
    ws.ws_col = cols;
    ws.ws_row = rows;

    /* Set up proper terminal attributes */
    struct termios term;
    memset(&term, 0, sizeof(term));
    /* Input modes */
    term.c_iflag = ICRNL | IXON | IXANY | IMAXBEL | IUTF8;
    /* Output modes - ONLCR maps NL to CR-NL */
    term.c_oflag = OPOST | ONLCR;
    /* Control modes */
    term.c_cflag = CREAD | CS8 | HUPCL;
    /* Local modes */
    term.c_lflag = ICANON | ISIG | IEXTEN | ECHO | ECHOE | ECHOK | ECHOKE | ECHOCTL;
    /* Control characters */
    term.c_cc[VEOF] = 4;      /* Ctrl-D */
    term.c_cc[VEOL] = 255;
    term.c_cc[VERASE] = 127;  /* DEL */
    term.c_cc[VINTR] = 3;     /* Ctrl-C */
    term.c_cc[VKILL] = 21;    /* Ctrl-U */
    term.c_cc[VMIN] = 1;
    term.c_cc[VQUIT] = 28;    /* Ctrl-\ */
    term.c_cc[VSTART] = 17;   /* Ctrl-Q */
    term.c_cc[VSTOP] = 19;    /* Ctrl-S */
    term.c_cc[VSUSP] = 26;    /* Ctrl-Z */
    term.c_cc[VTIME] = 0;
    /* Set baud rate */
    cfsetispeed(&term, B38400);
    cfsetospeed(&term, B38400);

    pty->child_pid = forkpty(&pty->master_fd, NULL, &term, &ws);

    if (pty->child_pid == 0) {
        /* Child process - exec shell */
        const char *shell_path = lean_string_cstr(shell);

        /* Set up environment */
        setenv("TERM", "xterm-256color", 1);
        setenv("COLORTERM", "truecolor", 1);
        /* Disable zsh's partial line indicator - it always shows because zsh can't
           reliably detect cursor position after child processes run */
        setenv("PROMPT_EOL_MARK", "", 1);

        /* Execute shell */
        execlp(shell_path, shell_path, "-l", NULL);

        /* If exec fails, exit */
        _exit(127);
    } else if (pty->child_pid < 0) {
        free(pty);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("forkpty failed")));
    }

    /* Parent process - set non-blocking mode */
    int flags = fcntl(pty->master_fd, F_GETFL, 0);
    if (flags >= 0) {
        fcntl(pty->master_fd, F_SETFL, flags | O_NONBLOCK);
    }

    return lean_io_result_mk_ok(vane_pty_box(pty));
}

/*
 * Read from PTY (non-blocking)
 *
 * @param pty PTY handle
 * @param max_bytes Maximum bytes to read
 * @return ByteArray with data read (may be empty if no data available)
 */
LEAN_EXPORT lean_obj_res vane_pty_read(
    b_lean_obj_arg pty_obj,
    uint32_t max_bytes,
    lean_obj_arg world
) {
    vane_pty_t *pty = vane_pty_unbox(pty_obj);

    if (pty->master_fd < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("PTY is closed")));
    }

    uint8_t *buffer = (uint8_t *)malloc(max_bytes);
    if (!buffer) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate read buffer")));
    }

    ssize_t n = read(pty->master_fd, buffer, max_bytes);

    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            /* No data available - return empty array */
            n = 0;
        } else {
            free(buffer);
            return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string(strerror(errno))));
        }
    }

    /* Create Lean ByteArray */
    lean_object *arr = lean_alloc_sarray(1, n, n);
    if (n > 0) {
        memcpy(lean_sarray_cptr(arr), buffer, n);
    }
    free(buffer);

    return lean_io_result_mk_ok(arr);
}

/*
 * Write to PTY
 *
 * @param pty PTY handle
 * @param data ByteArray to write
 * @return Unit or IO error
 */
LEAN_EXPORT lean_obj_res vane_pty_write(
    b_lean_obj_arg pty_obj,
    b_lean_obj_arg data,
    lean_obj_arg world
) {
    vane_pty_t *pty = vane_pty_unbox(pty_obj);

    if (pty->master_fd < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("PTY is closed")));
    }

    size_t len = lean_sarray_size(data);
    const uint8_t *ptr = lean_sarray_cptr(data);

    size_t written = 0;
    while (written < len) {
        ssize_t n = write(pty->master_fd, ptr + written, len - written);
        if (n < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                /* Would block - retry */
                continue;
            }
            return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string(strerror(errno))));
        }
        written += n;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * Resize PTY
 *
 * @param pty PTY handle
 * @param cols New width in columns
 * @param rows New height in rows
 * @return Unit or IO error
 */
LEAN_EXPORT lean_obj_res vane_pty_resize(
    b_lean_obj_arg pty_obj,
    uint16_t cols,
    uint16_t rows,
    lean_obj_arg world
) {
    vane_pty_t *pty = vane_pty_unbox(pty_obj);

    if (pty->master_fd < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("PTY is closed")));
    }

    struct winsize ws;
    memset(&ws, 0, sizeof(ws));
    ws.ws_col = cols;
    ws.ws_row = rows;

    if (ioctl(pty->master_fd, TIOCSWINSZ, &ws) < 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("ioctl TIOCSWINSZ failed")));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * Poll for data availability
 *
 * @param pty PTY handle
 * @param timeout_ms Timeout in milliseconds (0 for non-blocking check)
 * @return Bool indicating if data is available
 */
LEAN_EXPORT lean_obj_res vane_pty_poll(
    b_lean_obj_arg pty_obj,
    uint32_t timeout_ms,
    lean_obj_arg world
) {
    vane_pty_t *pty = vane_pty_unbox(pty_obj);

    if (pty->master_fd < 0) {
        return lean_io_result_mk_ok(lean_box(0)); /* false */
    }

    struct pollfd pfd;
    pfd.fd = pty->master_fd;
    pfd.events = POLLIN;
    pfd.revents = 0;

    int ret = poll(&pfd, 1, timeout_ms);

    return lean_io_result_mk_ok(lean_box(ret > 0 && (pfd.revents & POLLIN)));
}

/*
 * Close PTY
 *
 * @param pty PTY handle (consumed)
 * @return Unit or IO error
 */
LEAN_EXPORT lean_obj_res vane_pty_close(
    lean_obj_arg pty_obj,
    lean_obj_arg world
) {
    vane_pty_t *pty = vane_pty_unbox(pty_obj);

    if (pty->master_fd >= 0) {
        close(pty->master_fd);
        pty->master_fd = -1;
    }

    if (pty->child_pid > 0) {
        kill(pty->child_pid, SIGHUP);
        waitpid(pty->child_pid, NULL, 0);
        pty->child_pid = -1;
    }

    lean_dec_ref(pty_obj);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * Check if child process is still alive
 *
 * @param pty PTY handle
 * @return Bool indicating if child is alive
 */
LEAN_EXPORT lean_obj_res vane_pty_is_alive(
    b_lean_obj_arg pty_obj,
    lean_obj_arg world
) {
    vane_pty_t *pty = vane_pty_unbox(pty_obj);

    if (pty->child_pid <= 0) {
        return lean_io_result_mk_ok(lean_box(0)); /* false */
    }

    int status;
    pid_t result = waitpid(pty->child_pid, &status, WNOHANG);

    if (result == 0) {
        /* Child still running */
        return lean_io_result_mk_ok(lean_box(1)); /* true */
    } else {
        /* Child exited or error */
        pty->child_pid = -1;
        return lean_io_result_mk_ok(lean_box(0)); /* false */
    }
}
