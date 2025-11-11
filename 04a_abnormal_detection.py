# -*- coding: utf-8 -*-
"""
@author: Ghamkhar, Khoshqalb
"""

# Libraries
import pandas as pd
import numpy as np
from tqdm import tqdm
import math
from sklearn.cluster import DBSCAN
from kneed import KneeLocator
from sklearn.neighbors import NearestNeighbors



def LZCn(x, b):
    """
    Calculate the Lempel-Ziv complexity of a sequence x with bin size b.

    Parameters:
    - x (array-like): Input sequence.
    - b (int): bin size.

    Returns:
    - float: Lempel-Ziv complexity.
    """
    X = np.asarray(x)
    bins = b

    bins = np.linspace(np.min(X), np.max(X), bins+1)[1:]
    sequence = np.searchsorted(bins, X, side="left")

    sub_strings = set()
    n = len(sequence)

    i = 0
    j = 1
    while i + j <= n:
        sub_str = tuple(sequence[i : i + j])  # convert to tuple to make it hashable
        if sub_str in sub_strings:
            j += 1
        else:
            sub_strings.add(sub_str)
            i += j
            j = 1
    
    if n <= 1 or b <= 1:
        e = float('inf')
    else:
        e = 2*((1+math.log(math.log(b*n, b), b))/ math.log(n, b))
    CU = abs(n/((1-e)*math.log(n, b)))
    CN = len(sub_strings) / CU

    return CN

def LZC(x, b):
    """
    Calculate the Lempel-Ziv complexity of a sequence x with bin size b.

    Parameters:
    - x (array-like): Input sequence.
    - b (int): bin size.

    Returns:
    - float: Lempel-Ziv complexity.
    """
    X = np.asarray(x)
    bins = b

    bins = np.linspace(np.min(X), np.max(X), bins+1)[1:]
    sequence = np.searchsorted(bins, X, side="left")

    sub_strings = set()
    n = len(sequence)

    i = 0
    j = 1
    while i + j <= n:
        sub_str = tuple(sequence[i : i + j])  # convert to tuple to make it hashable
        if sub_str in sub_strings:
            j += 1
        else:
            sub_strings.add(sub_str)
            i += j
            j = 1
    CN = (len(sub_strings) / n)
    
    return CN

def TSLF(x):
    """
    Calculate the time series length factor of a sequence x.

    Parameters:
    - x (array-like): Input sequence.

    Returns:
    - float: Time series length factor.
    """
    if not isinstance(x, (np.ndarray, pd.Series)):
        x = np.asarray(x)
        
    if x.size == 0:
        return np.nan
    
    return np.unique(x).size / x.size

# feature extraction process
def feature_extract(df):
    """
    Extract features including time series length factor and Lempel-Ziv complexity
    for different bin sizes from the DataFrame.

    Parameters:
    - df (DataFrame): Input DataFrame containing 'id' and 'value' columns.

    Returns:
    - DataFrame: DataFrame with extracted features.
    """
    # features for LZC
    mbin = np.array([2, 4, 9, 99])
    df_LZC = {}
    for id, group in tqdm(df.groupby('id')):
        x = group['value'].values
        CN = [TSLF(x)]
        CN += [LZC(x, b) if b == 1 else LZCn(x, b) for b in mbin] #Markus: From my understanding this is unneded as b = [2,4,9,99]
        df_LZC[id] = CN
        
    return pd.DataFrame.from_dict(df_LZC, orient='index', columns=[0, 2, 4, 9, 99])


def eps_calc(df, minpts):
    """
    Calculate the optimal epsilon value for DBSCAN clustering.

    Parameters:
    - df (DataFrame): Input DataFrame containing features.
    - minpts (int): Minimum number of points required to form a dense region.

    Returns:
    - float: Optimal epsilon value.
    """
    X = df.to_numpy()
    nearest_neighbors = NearestNeighbors(n_neighbors=minpts-1)
    neighbors = nearest_neighbors.fit(X)
    distances, indices = neighbors.kneighbors(X)

    distances = np.sort(distances[:,-1], axis=0)
    ld = np.arange(len(distances))
    knee = KneeLocator(ld, distances, S=1, curve='convex', direction='increasing', interp_method='polynomial')
    
    return knee.knee_y

def init_dbscan(df, minpts, eps):
    """
    Initialize DBSCAN clustering with given parameters.

    Parameters:
    - df (DataFrame): Input DataFrame containing features.
    - minpts (int): Minimum number of points required to form a dense region.
    - eps (float): Epsilon value for DBSCAN.

    Returns:
    - DataFrame: DataFrame with DBSCAN clustering results.
    """
    # initial DBSCAN
    X = df.to_numpy()
    db = DBSCAN(eps=eps, min_samples=minpts).fit(X)
    core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
    core_samples_mask[db.core_sample_indices_] = True
    labels = db.labels_

    X = pd.DataFrame(X)
    X['eps'] = eps
    X['DBSCAN'] = labels
    X['IDs'] = df.index.values
    
    return X

 
def get_result_labeled(df, minpts):
    """
    Get labeled clustering result using DBSCAN algorithm.

    Parameters:
    - df (DataFrame): Input DataFrame containing features.
    - minpts (int): Minimum number of points required to form a dense region.

    Returns:
    - DataFrame: DataFrame with labeled cluster assignments for DBSCAN clustering.
    """
    # Extract features
    df_LZC = feature_extract(df)
    df_LZC_clean = df_LZC.dropna()

    # Calculate optimal epsilon value
    eps = eps_calc(df_LZC_clean, minpts)

    # Initialize DBSCAN clustering and get labeled cluster assignments
    result_labeled = init_dbscan(df_LZC_clean, minpts, eps)
    return result_labeled

def get_selected_ids(df, target_value):
    """
    Get IDs of data points assigned to a specific cluster label.

    Parameters:
    - df (DataFrame): Input DataFrame containing cluster labels and IDs.
    - target_value (int): Target cluster label.

    Returns:
    - Series or None: Series of IDs for data points assigned to the target cluster label, or None if no points are found.
    """
    selected_ids = df[df['DBSCAN'] == target_value]['IDs']
    if not selected_ids.empty:
        return selected_ids
    else:
        return None

def find_nearest_value(array, value):
    """
    Find the nearest value in an array to a given value.

    Parameters:
    - array (array-like): Input array or list of values.
    - value: Value to find the nearest value to.

    Returns:
    - float: Nearest value in the array to the given value.
    """
    idx = np.argmin(np.abs(array - value))
    
    return array[idx]

def process_dictionaries(result_labeled, result_share):
    """
    Process labeled clustering results and share statistics to analyze clusters.

    Parameters:
    - result_labeled (DataFrame): DataFrame containing labeled cluster assignments.
    - result_share (DataFrame): DataFrame containing cluster statistics.

    Returns:
    - 
    """
    labeled_df = result_labeled
    eps_value = labeled_df['eps'].values[0]

    share_df = result_share
    distances = share_df['distances'].values

    nearest_eps = find_nearest_value(distances, eps_value)

    nearest_row = share_df[share_df['distances'] == nearest_eps]

    clusters = nearest_row['n_clusters_'].values[0]

    if clusters > 2:
        two_cluster_distances = distances[share_df['n_clusters_'] == 2]
        nearest_two_cluster_distance = find_nearest_value(two_cluster_distances, nearest_eps)
        print(f"Nearest 'eps' value in result_share: {nearest_eps}")
        print(f"Clusters at nearest 'eps' value: {clusters}")
        print(f"Nearest 'distances' value with 2 clusters: {nearest_two_cluster_distance}\n")

    
def main_fd(input_data): 
    # Read dataset
    df_mr_clean = input_data
    
    # Get labeled clustering result using DBSCAN algorithm
    result_labeled = get_result_labeled(df_mr_clean, 15)
    
    # Get IDs of data points assigned to a specific cluster label
    selected_ids_dict = get_selected_ids(result_labeled, -1)
    
    selected_ids_pd = selected_ids_dict.to_frame()
    selected_ids_pd.rename(columns={'IDs': 'id'}, inplace=True)
    return(selected_ids_pd)
