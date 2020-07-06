# -*- coding: utf-8 -*-
"""
data2database
    Load and transfer logsheet, subject dimensions, digitized, and force data
        to database.

This code will also create a table that records the table names of each cycle.

Should begin in 'wheelchair_graded' folder.

Created on Mon Jul  6 08:51:24 2020

@author: cwiens
"""

import sqlite3
import pandas as pd
import glob, os

from codes.importlogsheet import readls


""" set file names """
db = '../data/wc_graded.sqlite'
logsheet = '../data/logsheet_master.xlsx'
sub_dim_file = '../data/Subject Body Dimensions_Complete2.xlsx'


""" establish database connection """
conn = sqlite3.connect(db)


""" load master logsheet """
ls = readls(logsheet)
sub_dim = pd.read_excel(sub_dim_file).iloc[:,:-1]


""" upload logsheet to database """
ls.to_sql('logsheet', conn, if_exists='replace')
sub_dim.to_sql('subject_dimensions', conn, if_exists='replace')


""" load digitized and force data """
# intialzie table_data
table_data = None
# change folder to where data is stored
os.chdir('../data/subject_digi_force')
# loop through each file
for file in glob.glob('*.xlsx'):
    data_digi = pd.read_excel(file).iloc[:,:-3]
    data_force = pd.read_excel(file).iloc[:,-3:]
    # find cycle info
    sub = '{:02}'.format(int(''.join(filter(str.isdigit, file[:7]))))
    des = ''.join(filter(str.isdigit, file[8:21]))
    cond = ''.join(filter(str.isdigit, file[22:33]))
    trial = '{:02}'.format(int(''.join(filter(str.isdigit, file[33:40]))))
    cycle = '{:02}'.format(int(''.join(filter(str.isdigit, file[40:]))))
    """ upload force data to database """
    data_digi.to_sql('digi_' + sub + des + cond + trial + cycle, conn)
    data_force.to_sql('force_' + sub + des + cond + trial + cycle, conn)
    """ add data info to list of tables """
    if table_data is None:
        table_data = pd.DataFrame({'subject_id': [int(''.join(filter(str.isdigit, file[:7])))],
                                               'description': [int(''.join(filter(str.isdigit, file[8:21])))],
                                               'condition': [int(''.join(filter(str.isdigit, file[22:33])))],
                                               'trial': [int(''.join(filter(str.isdigit, file[33:40])))],
                                               'cycle': [int(''.join(filter(str.isdigit, file[40:])))],
                                               'digi': ['digi_' + sub + des + cond + trial + cycle],
                                               'force': ['force_' + sub + des + cond + trial + cycle]})
    else:
        table_data = table_data.append(pd.DataFrame({'subject_id': [int(''.join(filter(str.isdigit, file[:7])))],
                                                     'description': [int(''.join(filter(str.isdigit, file[8:21])))],
                                                     'condition': [int(''.join(filter(str.isdigit, file[22:33])))],
                                                     'trial': [int(''.join(filter(str.isdigit, file[33:40])))],
                                                     'cycle': [int(''.join(filter(str.isdigit, file[40:])))],
                                                     'digi': ['digi_' + sub + des + cond + trial + cycle],
                                                     'force': ['force_' + sub + des + cond + trial + cycle]})).reset_index(drop=True)
            
# return to original folder
os.chdir('../..')
# upload table_data
table_data.to_sql('table_list', conn)


""" close connection """
conn.close()
