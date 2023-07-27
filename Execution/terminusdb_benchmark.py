from requests.auth import HTTPBasicAuth
from python_graphql_client import GraphqlClient
import csv
import json
import time

GRAPHQL_ENDPOINT="http://localhost:6363/api/graphql/admin/wdbench"
GRAPHQL_FILES=["Queries/GraphQL/single_bgps.txt",
               "Queries/GraphQL/multiple_bgps.txt"]

# Instantiate the client with an endpoint.
auth = HTTPBasicAuth('admin', 'root')
client = GraphqlClient(endpoint=GRAPHQL_ENDPOINT,auth=auth)

for gf in GRAPHQL_FILES:
    with open(gf, 'r') as qf:
        print(f"Running queries: {gf}")
        with open(f"{gf}.times",'w') as tf:
            csvreader = csv.reader(qf)
            csvwriter = csv.writer(tf)
            for row in csvreader:
                n = row[0]
                query = row[1]
                query = "query{" + query +"}"
                print(f"..{n}")
                # Synchronous request
                start_time = time.time()
                data = client.execute(query=query)
                elapsed_time = int((time.time() - start_time) * 1000)
                csvwriter.writerow((n,elapsed_time))
                if 'errors' in data:
                    print(data)
                else:
                    results = len(data['data']['Node'])
                    print(f"results: {results}")
