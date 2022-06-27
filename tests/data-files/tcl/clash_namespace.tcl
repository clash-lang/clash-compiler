namespace eval clash {
    # Populate a namespace with a Clash-generated Tcl interface.
    # Namespace is clash::tclIface::$top::$baseName
    proc loadTclIface {top clashTclFile} {
        # Evaluate script code inside temporary throwaway namespace to
        # separate its definitions from ours and reduce the chance of
        # accidentally corrupting our code.
        namespace eval tmp "source $clashTclFile"
        set baseName [file rootname [file tail $clashTclFile]]
        tmp::createNamespace [namespace current]::tclIface::${top}::${baseName}
        namespace delete tmp
    }

    proc runClashScripts {} {
        if {![namespace exists tclIface]} {
            # There are no scripts
            return
        }

        # Identical names means identical IP, only one run needed even if it
        # occurs in multiple HDL directories.
        set seen [list]
        foreach topNs [namespace children tclIface] {
            foreach tclIface [namespace children $topNs] {
                set api [subst $${tclIface}::api]
                if {$api ne {1}} {
                    error "Error: $tclIface doesn't implement an API we\
                        support: api = \"$api\"."
                }
                set purpose [subst $${tclIface}::scriptPurpose]
                if {$purpose ne {createIp}} {
                    error "Error: ${tclIface}::scriptPurpose bogus value\
                        \"$purpose\"."
                }
                set ipName [subst $${tclIface}::ipName]
                if {$ipName in $seen} {
                    continue
                }
                ${tclIface}::createIp $ipName -dir ip
                lappend seen $ipName
            }
        }
        return $seen
    }
}
