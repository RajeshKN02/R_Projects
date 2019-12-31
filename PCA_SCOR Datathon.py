import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

qs = pd.read_csv("/Users/Tim/NHANES-Downloader-master/data/csv_data/2007-2008/Questionnaire/MCQ_E.csv")
qs = qs[['SEQN','MCQ160E']]
bd = pd.read_csv("/Users/Tim/NHANES-Downloader-master/data/csv_data/2007-2008/Examination/BMX_E.csv")
percent_missing = bd.isnull().sum() * 100 / len(bd)
missing_value_bd = pd.DataFrame({'column_name': bd.columns,
                                 'percent_missing': percent_missing})
#print(missing_value_bd)
for index,row in missing_value_bd.iterrows():
    if row['percent_missing'] > 80:
        bd = bd.drop(columns=row['column_name'])

df = qs.merge(bd, on='SEQN')
df = df.dropna(axis=0, subset=['MCQ160E'])
df['MCQ160E'] = df['MCQ160E'].apply(str)
df = df.replace({'MCQ160E': {'1.0': 'YES', '2.0': 'NO', '9.0': 'DK'}})

#replace the missing values with mean
df = df.fillna(df.mean()) #fillna with mean

#
df.to_csv("/Users/Tim/Desktop/SCOR/test/data_prepared.csv")

features = list(df.columns)
features = features[3::]
x = df.loc[:, features].values
y = df.loc[:,['MCQ160E']].values
x = StandardScaler().fit_transform(x)

pca = PCA(n_components=6)
principalComponents = pca.fit_transform(x)
principalDf = pd.DataFrame(data = principalComponents, columns = ['principal component 1', 'principal component 2',
                                                                  'principal component 3', 'principal component 4',
                                                                  'principal component 5', 'principal component 6'])
finalDf = pd.concat([principalDf, df[['MCQ160E']]], axis = 1)


# fig = plt.figure(figsize = (8,8))
# ax = fig.add_subplot(1,1,1)
# ax.set_xlabel('Principal Component 1', fontsize = 15)
# ax.set_ylabel('Principal Component 2', fontsize = 15)
# ax.set_title('2 Component PCA', fontsize = 20)
#
#
# targets = ["YES", "NO", "DK"]
# colors = ['r', 'g', 'b']
# for target, color in zip(targets,colors):
#     indicesToKeep = finalDf['MCQ160E'] == target
#     ax.scatter(finalDf.loc[indicesToKeep, 'principal component 1']
#                , finalDf.loc[indicesToKeep, 'principal component 2']
#                , c = color
#                , s = 50)
# ax.legend(targets)
# ax.grid(True)
# plt.show()

print(pca.explained_variance_ratio_)

print(pd.DataFrame(pca.components_,columns=features,index = ['PC-1','PC-2','PC-3','PC-4','PC-5','PC-6']))
