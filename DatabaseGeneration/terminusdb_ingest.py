#!/usr/bin/env python3
import argparse
import subprocess
import threading
import os
import json
import sys
import time
from multiprocessing import Pool

TERMINUSDB_COMMAND = 'terminusdb'

def prefix_number(number):
    str_number = str(number)
    if number < 10:
        return "00" + str_number
    elif number < 100:
        return "0" + str_number
    return str_number

def init_db(schema, threads):
    for x in range(0, threads):
        number = prefix_number(x)
        try:
            #subprocess.run(f"{TERMINUSDB_COMMAND} db delete admin/wdbench_{number}", shell=True)
            subprocess.run(f'{TERMINUSDB_COMMAND} db create admin/wdbench_{number} --schema=false', shell=True)
            pass
        except:
            subprocess.run(f"{TERMINUSDB_COMMAND} db delete admin/wdbench_{number}", shell=True)
            subprocess.run(f"{TERMINUSDB_COMMAND} db create admin/wdbench_{number} --schema=false", shell=True)
    #subprocess.run(f"{TERMINUSDB_COMMAND} db delete admin/wdbench", shell=True)
    #subprocess.run(f"{TERMINUSDB_COMMAND} doc insert admin/wdbench -g schema --full-replace < {schema}", shell=True)
    pass

def split_json(threads, filename):
    subprocess.run(f"split -n l/{threads} -d -a 3 {filename} Data/splits/wdbench_split", shell=True)
    subprocess.run(f'for i in `ls Data/splits/`; do cat Data/header.nt "Data/splits/$i" > "Data/splits/$i.part"; mv Data/splits/"$i.part" Data/splits/"$i"; done', shell=True)

def ingest_json(args):
    start = time.time()
    filename = args[0]
    number = args[1]
    #schema = args[2]
    db_name = f"wdbench_{number}"
    db = f'admin/{db_name}'
    with open(f"log/{db_name}.log", 'w') as f:
        #subprocess.run(f"{TERMINUSDB_COMMAND} doc insert {db} -g schema --full-replace < {schema}", shell=True, stdout=f, stderr=f)
        print(f'executing: {TERMINUSDB_COMMAND} triples load {db}/local/branch/main/instance Data/splits/{filename}')
        subprocess.run(f'{TERMINUSDB_COMMAND} triples load {db}/local/branch/main/instance Data/splits/{filename}', shell=True, stdout=f, stderr=f)
        end_insert = time.time() - start
        f.write(f"\n\nEND TIME: {end_insert}\n")
#    print(f"THREAD {number} finished inserting in: {end_insert} seconds")
#    start = time.time()
#    subprocess.run(f'sudo docker exec -i terminusdb /bin/bash -c \'./terminusdb triples dump {db}/local/branch/main/instance > {db_name}.triples\'', shell=True)
#    end_triples = time.time() - start
#    print(f"THREAD {number} dumped triples in: {end_triples} seconds")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=os.path.abspath)
    #parser.add_argument("schema", type=os.path.abspath)
    parser.add_argument("split", type=int)
    parser.add_argument("threads", type=int)
    args = parser.parse_args()
    print("Initializing databases")
    #init_db(None, args.split)
    print("Spliting")
    split_json(args.split, args.file)
    threads = []
    args_process = [('wdbench_split' + prefix_number(x), prefix_number(x), None) for x in range(0, args.split)]
    with Pool(args.threads) as p:
        pass
        p.map(ingest_json, args_process)
    print("completed partitioned import")
    dbs = ""
    for x in range(0,args.split):
        dbs += f"admin/wdbench_{x:03d}\n"
    # TODO: We have to squash all the different data products into one
    try:
        subprocess.run(f'{TERMINUSDB_COMMAND} db delete admin/wdbench')
    except:
        pass
    subprocess.run(f'{TERMINUSDB_COMMAND} db create admin/wdbench --schema=false', shell=True)
    print("Merging database")
    reset_output =subprocess.run(f'{TERMINUSDB_COMMAND} concat admin/wdbench', text=True, shell=True, input=dbs, capture_output=True)
    reset_id = reset_output.stdout.split('\n')[0]
    subprocess.run(f'{TERMINUSDB_COMMAND} reset admin/wdbench {reset_id}', text=True, shell=True)
    print("Loading schema")
    subprocess.run(f'{TERMINUSDB_COMMAND} doc insert -f -g schema admin/wdbench < Data/terminusdb-schema.json', text=True, shell=True)

if __name__ == '__main__':
    main()
