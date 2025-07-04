# Path to the kakorn executable 
declare-option str kakorn_path
# The FIFO for the buffers
declare-option str kakorn_buf_fifo
# Used to determine if a buffer has updated
declare-option int kakorn_buf_update_timestamp
# Used to store the length of the buffer
declare-option int kakorn_buf_len

# Set the binary to the recently built one for testing
set-option global kakorn_path "/home/chameko/projects/acorn/_build/install/default/bin/kakorn"

# Add the acorn filetype
hook global BufCreate .*\.(acorn) %{
    set-option buffer filetype acorn
}

# A helper function that does nothing.
#
# Used with kakorn-exec-if-changed to have a fallback when the buffer has
# not changed. Taken from kak-tree-sitter.
define-command -hidden kakorn-exec-nop-0 nop

# A helper function that executes its argument only if the buffer has changed. Taken from kak-tree-sitter.
define-command -hidden kakorn-exec-if-changed -params 1 %{
    set-option -remove buffer kakorn_buf_update_timestamp %val{timestamp}

    try %{
        evaluate-commands "kakorn-exec-nop-%opt{tree_sitter_buf_update_timestamp}"
        set-option buffer kakorn_buf_update_timestamp %val{timestamp}
    } catch %{
        # Actually run the command
        set-option buffer kakorn_buf_update_timestamp %val{timestamp}
        evaluate-commands %arg{1}
    }
}

# Do our thing when an acorn file is opened
hook -group kakorn global BufSetOption filetype=(acorn) %{
    set-option buffer kakorn_buf_fifo %sh{$kak_opt_kakorn_path -r "(OpenBuffer (session $kak_session) (name $kak_bufname))"}

    # Update the buffer in normal mode
    hook -group kakorn buffer NormalIdle .* %{
        kakorn-exec-if-changed %{
            execute-keys -draft "ge:set-option buffer kakorn_buf_len %%val{cursor_byte_offset}<ret>"
            echo -to-file %opt{kakorn_buf_fifo} "%opt{kakorn_buf_len}
"
            write -force %opt{kakorn_buf_fifo}
        }
    }
    # Update the buffer in insert mode
    hook -group kakorn buffer InsertIdle .* %{
        kakorn-exec-if-changed %{
            execute-keys -draft "ge:set-option buffer kakorn_buf_len %%val{cursor_byte_offset}<ret>"
            echo -to-file %opt{kakorn_buf_fifo} "%opt{kakorn_buf_len}
"
            write -force %opt{kakorn_buf_fifo}
        }
    }
}

define-command -docstring "End the kakorn session" kakorn-end-session %{
    remove-hooks global kakorn
    nop %sh{$kak_opt_kakorn_path -r "Term"}
}

define-command -docstring "Start the kakorn session" kakorn-start-session %{
    %sh{
        $kak_opt_kakorn_path --server --daemonize --init $kak_session
    }
}

# Kill the server when we exit
hook global KakEnd .* %{
    kakorn-end-session
}
