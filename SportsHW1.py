#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep  6 19:16:34 2021

@author:
"""
import numpy as np
import pandas as pd


df = pd.read_csv("/Users/caseyb5712/Desktop/STAT_4800_dataset/2021 UVA Football Data-selected/2019 PFF All Plays.csv") #read in data
df.reindex(index=df.index[::-1])


fieldposition= []


for i in range(len(df)): 

    if df['pff_FIELDPOSITION'][i] < 0: 
        fieldposition.append(abs(df['pff_FIELDPOSITION'][i]))
        
    if df['pff_FIELDPOSITION'][i] > 0:
        fieldposition.append(50 + (50-df['pff_FIELDPOSITION'][i]))
            

df['fieldpos']= fieldposition



dfq1= df[df.pff_QUARTER == 1]
dfq3= df[df.pff_QUARTER == 3]
dfq1q3=pd.concat([dfq1, dfq3])

dfq1q3= dfq1q3[dfq1q3.pff_GARBAGETIME == 0 ]


dfq1q3 = dfq1q3.dropna(subset=['pff_DRIVE'])

dfq1q3= dfq1q3.reset_index(drop=True)


nextscore = 0
scoring_team = ''
current_game = 14902
score = []

for i in range(len(dfq1q3)): 
    if dfq1q3['pff_GAMEID'][i] != current_game:
        nextscore = 0
        current_game = dfq1q3['pff_GAMEID'][i]
        scoring_team= ''
    if dfq1q3['pff_DRIVEENDEVENT'][i] == 'TOUCHDOWN':
        nextscore = 6
        scoring_team = dfq1q3['pff_OFFTEAM'][i]
    if dfq1q3['pff_DRIVEENDEVENT'][i] == 'FIELD GOAL':
        nextscore = 3
        scoring_team = dfq1q3['pff_OFFTEAM'][i]
    if dfq1q3['pff_DRIVEENDEVENT'][i] == 'SAFETY':
        nextscore = 2
        scoring_team = dfq1q3['pff_DEFTEAM'][i]
    else:
        None
    if nextscore == 0:
        score.append(nextscore)
    elif dfq1q3['pff_OFFTEAM'][i] == scoring_team:
        score.append(nextscore)
    else:
        score.append(nextscore *-1)

dfq1q3['Next Score']= score

final_df= dfq1q3[dfq1q3['Next Score'] != 0]


def round10(x, base=10):
    return int(base * round(float(x)/base))


def round4(x, base=4):
    return int(base * round(float(x)/base))


final_df['Fieldpostens']= final_df['fieldpos'].apply(lambda x: round10(x, base=10))
final_df['Distanceround']= final_df['pff_DISTANCE'].apply(lambda x: round4(x, base=4))



final_df['State'] = final_df['pff_DOWN'].astype(str) + ',' +  final_df['Distanceround'].astype(str) + "," + final_df['Fieldpostens'].astype(str)
 


exp_points= final_df['Next Score'].groupby(final_df['State']).mean().sort_values(ascending=False)



down_input = 1
distance_input = 14
fieldpos_input = 55

dinput_round=  4 * round(distance_input/4)
finput_round= 10 * round(fieldpos_input/10)



function_input= str(down_input)+ ',' + str(dinput_round)+ ',' + str(finput_round)

rslt_df = exp_points.filter(items = [function_input], axis=0)

print(rslt_df)



