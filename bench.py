#!/usr/bin/env python3
import subprocess
import time


SED_REPLACE_CMD = r"s/-qn[0-9]+ -A[0-9]+M -RTS -j[0-9]+/-qn{qn} -A{mem}M -RTS -j{j}/g"

QNS = [2, 4, 8]
MEMS = [2, 4, 8, 16, 32, 64]
JS = [2, 4, 8, 16]


def sed(qn, mem, j):
    sed_cmd = SED_REPLACE_CMD.format(qn=qn, mem=mem, j=j)
    subprocess.check_call(["sed", "-E", "-i", sed_cmd, "cabal.project"])

def clean():
    subprocess.check_call(["cabal", "clean"])

def update():
    subprocess.check_call(["cabal", "update"])

def build():
    subprocess.check_call([
        "cabal", "build",
        "clash-prelude", "clash-lib", "clash-ghc", "clash-cores",
        "-w", "ghc-9.6.4"
    ])

results = []

with open("bench.csv", "w") as csv:
    # Warm up the caches
    update()
    build()

    csv.write("qn,mem,j,error,time\n")
    csv.flush()

    # Default if no flags set: useful as a baseline
    testcases = [(1, 4, 1)]

    for qn in QNS:
        for mem in MEMS:
            for j in JS:
                testcases.append((qn, mem, j))

    for (qn, mem, j) in testcases:
        sed(qn, mem, j)
        clean()
        update()

        before_time = time.time()
        error = 0
        try:
            build()
        except:
            error = 1
        after_time = time.time()

        elapsed_time = after_time - before_time
        csv.write(f"{qn},{mem},{j},{error},{elapsed_time}\n")
        csv.flush()

        results.append([qn,mem,j,error,elapsed_time])

with open("bench.md", "w") as md:
    md.write("| qn | mem | j | error | time |\n")
    md.write("|---|---|---|---|---|\n")
    results.sort(key=lambda r : r[-1])
    for row in results:
        md.write("| {} | {} | {} | {} | {:.2f} |\n".format(*row))
