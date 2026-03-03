import pandas as pd
import numpy as np

# Prepare the data: rename questions
def rename_items(df):
    rename_col_dict = {
        "codice_partecipante": "ID",
        "età": "Age",
        "Gnr": "Sex",
        "Istr": "Edu",
        "P_cultur_1_1": "A1",
        "P_cultur_1_2": "A2",
        "P_cultur_1_3": "A3",
        "P_cultur_1_4": "B1",
        "P_cultur_1_5": "B2",
        "P_cultur_1_6": "B3",	
        "P_cultur_1_7": "C1",	
        "P_cultur_1_8": "C2",	
        "P_cultur_1_9": "C3",	
        "P_cultur_1_10": "D1",	
        "P_cultur_1_11": "D2",	
        "P_cultur_1_12": "D3",	
        "P_cultur_1_13": "E1",	
        "P_cultur_1_14": "E2",	
        "P_cultur_1_15": "E3",	
        "P_cultur_1_16": "E4",	
        "Media Interessi Culturali": "Avg Chare"
    }

    df_clean = df.rename(columns=rename_col_dict)
    df_clean = df_clean.drop(columns=['Avg Chare'])
    return df_clean

def add_correlation_items(df, full_dataset, file_path):

    df_clean = rename_items(df)

    # Add the average of the artistic interests
    df_clean['Avg Art Experience'] = np.nan
    df_clean['Avg Art Activities'] = np.nan
    df_clean['Avg Art Recognition'] = np.nan

    for index, row in df_clean.iterrows():
        part_id = row['ID']

        #Q1 Art Experience
        val_art_exp_list = [
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_1'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_2'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_3'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_4'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_5'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_6'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_7'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_8'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_9'].values[0],
            full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Q1_10'].values[0]
        ]
        

        df_clean.at[index, 'Avg Art Experience'] = np.mean(val_art_exp_list)
            
        #Q2 Art Activities
        df_clean.at[index, 'Avg Art Activities'] = full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Questionario Interessi artistici media'].values[0]

        #Q3 Art Recognition
        df_clean.at[index, 'Avg Art Recognition'] = full_dataset.loc[full_dataset['codice_partecipante'] == part_id, 'Somma_Riconoscimento stili artistici'].values[0]
        df_clean.to_excel(file_path, index=False)

    return df_clean

chare_data = pd.read_excel('survey-2406/rawdata/chare.xlsx', sheet_name=0)
full_data = pd.read_excel('survey-2406/rawdata/general.xlsx', sheet_name=0)

chare_clean = add_correlation_items(chare_data, full_data, 'survey-2406/chare_clean.xlsx')