set breakpoint pending on
break barf
  commands
    printf "Hit barf breakpoint, generating coredump\n"
    generate-core-file
    disable 1
    continue
  end
break abort
  commands
    printf "Hit abort breakpoint. killing process\n"
    kill
    exit 1
  end
run
