# Copyright   :  (C) 2021-2022, QBayLogic B.V.,
#                    2022     , Google Inc.
# License     :  BSD2 (see the file LICENSE)
# Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

namespace eval clash {
    # Populate a namespace with a Clash-generated Tcl interface.
    # Namespace is clash::tclIface::$top::$baseName
    proc loadTclIface {top clashTclFile} {
        # Evaluate script code inside temporary throwaway namespace to
        # separate its definitions from ours and reduce the chance of
        # accidentally corrupting our code.
        namespace eval tmp "source $clashTclFile"
        if {[info procs tmp::createNamespace] eq {}} {
            error "Error: $clashTclFile does not provide a procedure named\
                \"createNamespace\". The Tcl script does not conform to the\
                defined Clash<->Tcl API."
        }
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
                # In Tcl, you can call procedures with a partial name. So an
                # invocation of "createIp" could call "createIpAlt" if
                # "createIp" did not exist. Let's be strict here to prevent
                # confusion: only accept the exact name "createIp".
                set createIp ${tclIface}::createIp
                if {[info procs $createIp] eq {}} {
                    error "Error: $tclIface doesn't provide \"createIp\"\
                        procedure."
            }

                set ipName [subst $${tclIface}::ipName]
                if {$ipName in $seen} {
                    continue
                }
                $createIp $ipName -dir ip
                lappend seen $ipName
            }
        }
        return $seen
    }
}
