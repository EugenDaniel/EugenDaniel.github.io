#!/usr/bin/env python
# coding: utf-8

# In[ ]:
import socket
import pandas as pd
import csv
import os
import pickle
from time import sleep
from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow,Flow
from google.auth.transport.requests import Request

# Pre requisits for the port and google sheets
host = 'localhost'
port = 9999
CLIENT_SECRET_FILE = 'credentials.json'
API_NAME = 'sheets'
API_VERSION = 'v4'
SCOPES = ['https://www.googleapis.com/auth/spreadsheets']
spreadsheet_id = '15DF2KnSUbyAK2HBmlhl7Z0bZysCn45tf7vH3rC43-p8'
SAMPLE_RANGE_NAME = 'A1:AA1000'

# Functions
def Create_Service(client_secret_file, api_service_name, api_version, *scopes):
    global service
    SCOPES = [scope for scope in scopes[0]]
    #print(SCOPES)
    
    cred = None

    if os.path.exists('token_write.pickle'):
        with open('token_write.pickle', 'rb') as token:
            cred = pickle.load(token)

    if not cred or not cred.valid:
        if cred and cred.expired and cred.refresh_token:
            cred.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(client_secret_file, SCOPES)
            cred = flow.run_local_server()

        with open('token_write.pickle', 'wb') as token:
            pickle.dump(cred, token)

    try:
        service = build(api_service_name, api_version, credentials=cred)
        print(api_service_name, 'service created successfully')
        #return service
    except Exception as e:
        print(e)
        #return None
        
# change 'my_json_file.json' by your downloaded JSON file.
Create_Service('credentials.json', 'sheets', 'v4',['https://www.googleapis.com/auth/spreadsheets'])
    
def Export_Data_To_Sheets(df):
    response_date = service.spreadsheets().values().append(
        spreadsheetId=spreadsheet_id,
        valueInputOption='RAW',
        range=SAMPLE_RANGE_NAME,
        insertDataOption='INSERT_ROWS',
        body=dict(
            majorDimension='COLUMNS',
            values=df.values.tolist())
    ).execute()
    print('Sheet successfully Updated')
    
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((host, port))
s.listen(1)

val = True

while val:
    print('Listening for a client at', host, port)
    conn, addr = s.accept()
    print('Connected by, addr')
    try:
        print('Reading file...\n')
        with open('Crimes_-_2001_to_Present.csv', 'r') as f:
            file = csv.reader(f,skipinitialspace=True)
            n = 0
            for line in file:
                if n == 0:
                    n += 1
                    df = pd.DataFrame(line)
                    Export_Data_To_Sheets(df)
                    continue
                data = [line[i] for i in (5,3,6,7,8,9)]
                data[-1] = data[-1] + '\n'
                if line[-2] == '':
                    continue
                else:
                    data = ','.join(data)
                    data = data.encode('utf-8')
                    df = pd.DataFrame(line)
                    if n == 1:
                        df_1 = df.copy()
                        n += 1
                        print(line)
                        print('-------------------------------')
                        conn.send(data)
                        sleep(0.15)
                    elif n > 1 and n < 50:
                        df_1 = pd.concat([df_1.reset_index(drop=True), df.reset_index(drop=True)], axis=1)
                        n += 1
                        print(line)
                        print('-------------------------------')
                        conn.send(data)
                        sleep(0.15)
                    else:
                        Export_Data_To_Sheets(df_1)
                        n = 1
                        print(line)
                        print('-------------------------------')
                        conn.send(data)
                        sleep(0.15)
            print('End of Stream')
    except socket.error:
        print('Error occurred.\n\nClient disconnected.\n')
        val = False
conn.close()

