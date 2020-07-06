# -*- coding: utf-8 -*-
"""
Created on Mon Jul  6 09:37:59 2020

@author: cwiens
"""

import pandas as pd
import numpy as np

def readls(logsheet):
    
    # read logsheet using third row as header
    ls = pd.read_excel(logsheet, header = 3)
    # find where new subject rows begin
    subjects = ls.iloc[:,0].dropna()
    # initialize ls_out
    ls_out = None
    
    for cnts in range(len(subjects)):
        # set start and end row
        row_s = subjects.index[cnts]
        if cnts != len(subjects)-1:
            row_e = subjects.index[cnts+1]-1
        else:
            row_e = len(ls)
        # create subset of loghsheet for only current subject
        ls_temp2 = ls.iloc[row_s:row_e].dropna(subset = ['Trial # '])
        ls_temp3 = ls.iloc[row_s:row_e].dropna(subset = ['Trial # .1'])
        
        # session 2
        ls_s2 = pd.DataFrame({'subject_num': int(subjects.iloc[cnts]),
                              'subject_id': ls_temp2['Subject ID'][row_s],
                              'session': 2,
                              'description': np.nan,
                              'condition': np.nan,
                              'trial_id': format(int(subjects.iloc[cnts]), '02') + str(2) + ls_temp2['Trial # '].apply(lambda x: '{:02}'.format(x)),
                              #'date': ls_temp2['Date.1'][row_s],
                              'days_btwn_sess': ls_temp2['Days b/n sessions'][row_s],
                              'trial_num': ls_temp2['Trial # '],
                              'force_file': ls_temp2['SmartWheel Force Data Format .csv'],
                              'opal_file': ls_temp2['Opal Sensor format .csv'],
                              'camera_file_front': ls_temp2['Front Camera JVC 60 fps format .MP4'],
                              'camera_file_side': ls_temp2['Side Camera JVC 60 fps format .MOV'],
                              'trial_type': ls_temp2['Trial Type'],
                              'wc_config': ls_temp2['Wheelchair Config'],
                              'comments_notes': ls_temp2['comments/ notes'],
                              'modification': ls_temp2['modification']})
        
        # session 3
        ls_s3 = pd.DataFrame({'subject_num': int(subjects.iloc[cnts]),
                              'subject_id': ls_temp3['Subject ID'][row_s],
                              'session': 3,
                              'description': 3,
                              'condition': np.nan,
                              'trial_id': format(int(subjects.iloc[cnts]), '02') + str(3) + ls_temp3['Trial # .1'].apply(lambda x: '{:02}'.format(x)),
                              #'date': ls_temp3['Date.2'][row_s],
                              'days_btwn_sess': ls_temp3['Days b/n sessions.1'][row_s],
                              'trial_num': ls_temp3['Trial # .1'],
                              'force_file': ls_temp3['SmartWheel Force Data Format .csv.1'],
                              'opal_file': ls_temp3['Opal Sensor format .csv.1'],
                              'camera_file_front': ls_temp3['Front Camera JVC 60 fps format .MP4.1'],
                              'camera_file_side': ls_temp3['Side Camera JVC 60 fps format .MP4'],
                              'trial_type': ls_temp3['Trial Type.1'],
                              'wc_config': ls_temp3['Wheelchair Config.1'],
                              'comments_notes': ls_temp3['comment'],
                              'modification': ls_temp3['comment.1']})
        
        # set description number
        ls_s2['description'][ls_s2['wc_config'].str.contains('(?i)baseline')] = 1
        ls_s2['description'][ls_s2['wc_config'].str.contains('(?i)new')] = 2
        # create full subject loghseet
        ls_sub = ls_s2.append(ls_s3)
        # set condition number
        ls_sub['condition'][ls_sub['trial_type'].str.contains('(?i)free')] = 1
        ls_sub['condition'][ls_sub['trial_type'].str.contains('(?i)fast')] = 2
        ls_sub['condition'][ls_sub['trial_type'].str.contains('(?i)graded')] = 3
        
        # combine with other subjects
        if ls_out is None:
            ls_out = ls_sub.reset_index(drop=True)
        else:
            ls_out = ls_out.append(ls_sub).reset_index(drop=True)
    
    
    return ls_out