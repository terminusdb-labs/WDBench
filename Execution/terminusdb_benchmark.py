from python_graphql_client import GraphqlClient
import csv
import json

GRAPHQL_ENDPOINT="https://localhost:6363/graphql/admin/wdbench"
GRAPHQL_FILES=["Queries/GraphQL/single_bgps.txt",
               "Queries/GraphQL/multiple_bgps.txt"]

# Instantiate the client with an endpoint.
client = GraphqlClient(endpoint=GRAPHQL_ENDPOINT)

for gf in GRAPHQL_FILES:
    with open(gf, 'r') as qf:
        with open(f"{gf}.times",'w') as tf:
            csvreader = csv.reader(qf)
            csvwriter = csv.writer(tf)
            for row in csvreader:
                n = row[0]
                query = row[1]
                print(f"running query {n}")
                # Synchronous request
                start_time = time.time()
                data = client.execute(query=query, variables=variables)
                elapsed_time = int((time.time() - start_time) * 1000)
                csvwriter.writerow((n,elapsed_time))
                js = json.dumps(data)
                print(f"data: {js}")
