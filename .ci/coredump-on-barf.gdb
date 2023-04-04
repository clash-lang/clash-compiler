set breakpoint pending on
break barf
  commands
    printf "Hit barf breakpoint, generating coredump\n"
    generate-core-file
    disable
    continue
  end
run
