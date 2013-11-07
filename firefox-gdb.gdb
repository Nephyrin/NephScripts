define hookpost-run
    if ($sigaction)
        call free($sigaction)
        set $sigaction = 0
    end
end

define check-asmjsfaulthandler
    if !$sigaction
        set $sigaction = malloc(sizeof(sigaction))
    end
    set $ignored = __sigaction(11, 0, $sigaction)
    set $handler = ((struct sigaction *)$sigaction)->__sigaction_handler.sa_handler
    if $handler == AsmJSFaultHandler
        continue
    end
end

define hook-stop
    if !$asmfaulthandlerinstalled
        set $asmfaulthandlerinstalled = 1
        echo Installing AsmJSFaultHandler catchpoint\n
        catch signal SIGSEGV
        commands
            silent
            check-asmjsfaulthandler
        end
    end
end

handle SIGPIPE nostop noprint pass
