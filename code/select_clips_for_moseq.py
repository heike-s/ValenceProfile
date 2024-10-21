
import glob, shutil
import pandas as pd

# Set up variables
project_folder = '/home/bagotlab/heike.s/Data/AppAv_CSDS_2309/'
cue_dict = {'1':'CSRp','2':'CSSp','3':'CSmm',
            '5':'CSRm','6':'CSSm'}

days = glob.glob(project_folder + 'data_preproc/dlc_output/Day*')
master = pd.read_csv(project_folder + 'master.csv')
design = pd.read_csv(project_folder + 'design.csv')

copy_list_og = []
copy_list_filtered = []
selected_all = []
for day in days:
    print(day)
    n_day = day[-2:]
    cues = glob.glob(day + '/*Day*.csv')
    cues_all = []
    for c in cues:
        df = pd.read_csv(c, index_col=None, header=0)
        df['id'] = c[-13:-10]
        cues_all.append(df)
        
    cues_all = pd.concat(cues_all, axis=0, ignore_index=True)
    cues_all.rename(columns={ cues_all.columns[0]: 'n' }, inplace = True)
    cues_all['id'] = cues_all['id'].astype(int)
    cues_all['cue_type_short'] = cues_all['cue_type'].str[0:3]
    cues_all = pd.merge(cues_all, master, how = 'left', left_on = 'id', right_on = 'ID')
    cues_all['Box'] = cues_all['Box'].astype('category')
    selected = cues_all.groupby(['Box','Sex','cue_type_short']).apply(lambda x: x.sample(2, random_state = 85215)).reset_index(drop=True)
    selected_all.append(selected)
    print(len(selected))
    for i in range(len(selected)):
        i_id = selected['id'].iloc[i].astype(str)
        i_n = selected['n'].iloc[i]
        i_n = ['0' + i_n.astype(str) if i_n < 10 else i_n.astype(str)][0]
        i_cue = selected['cue_type'].iloc[i]
        clipname = i_id + '_D' + n_day + '_' + i_n + '_' + i_cue
        copy_list_og.extend(glob.glob(day + '/' + clipname + '*shuffle1_*0.h5'))
        copy_list_filtered.extend(glob.glob(day + '/' + clipname + '*shuffle1_*filtered.h5'))

dest_og = '/home/bagotlab/heike.s/Data/ValenceProfile/kpmoseq_model/training_videos/unfiltered/'
for file in copy_list_og:
    shutil.copy(file, dest_og)

dest_filtered = '/home/bagotlab/heike.s/Data/ValenceProfile/kpmoseq_model/training_videos/filtered/'
for file in copy_list_filtered:
    shutil.copy(file, dest_filtered)

# Make videos and save