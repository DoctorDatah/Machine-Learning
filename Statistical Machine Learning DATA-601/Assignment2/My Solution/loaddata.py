import os
import numpy as np
import pandas as pd 
from zipfile import ZipFile
from kaggle.api.kaggle_api_extended import KaggleApi
from sklearn.model_selection import StratifiedShuffleSplit

 
PROJECT_ROOT = os.getcwd()

RAW_DATA_DIR = os.path.join(PROJECT_ROOT, "data/raw")
DATASET_NAME_KAGGLE = "titanic"

RAW_FILE_PATH = os.path.join(RAW_DATA_DIR, "titanic.zip")
RAW_TRAIN_PATH = os.path.join(RAW_DATA_DIR, "train.csv")
RAW_TEST_PATH = os.path.join(RAW_DATA_DIR, "test.csv")
RAW_DISPLAY_PATH = "/data/raw"

INTERM_DATA_DIR = os.path.join(PROJECT_ROOT, "data/intermi")
INTERM_TRAIN_PATH = os.path.join(INTERM_DATA_DIR, "train.csv")
INTERM_TEST_PATH = os.path.join(INTERM_DATA_DIR, "test.csv")


def download_titanic_data_from_kaggle(redownload_flag = False):
    """
    Make sure you have added kaggle.json file in .kaggle folder in Users/username folder.
    """
    if redownload_flag is True or os.path.exists(RAW_FILE_PATH) is False:
        api = KaggleApi()
        api.authenticate()
        os.chdir(RAW_DATA_DIR)
        files = api.competition_download_files(DATASET_NAME_KAGGLE)
        print("Dataset downloaded at " + RAW_DISPLAY_PATH)
    else: 
        print("Dataset already downloaded at " + RAW_DISPLAY_PATH)
        
def unzip_data(unzip_again_flag = False):
    if unzip_again_flag is True or os.path.exists(RAW_TRAIN_PATH) is False and os.path.exists(RAW_TEST_PATH) is False:
        zip_file = ZipFile(RAW_FILE_PATH)
        zip_file.extractall(path=RAW_DATA_DIR)
        zip_file.close()
        print("Data Unziped at " + RAW_DISPLAY_PATH)
    else:
        print("Data is already unziped at " + RAW_DISPLAY_PATH)

def load_raw_data():
    return pd.read_csv(RAW_TRAIN_PATH)