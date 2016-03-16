import pandas as pd
import numpy as np
import scipy
from sklearn import svm, cross_validation
from sklearn.preprocessing import LabelEncoder

train = pd.DataFrame(pd.read_csv('train-preprocessed.csv'))

for col in train.columns:
    if train[col].dtype == 'object':
        le = LabelEncoder()
        train[col] = le.fit_transform(train[col])
        train[col] = train[col].astype('category')
train['Survived'] = train['Survived'].astype('category')

x = train.iloc[:, 1:]
y = train['Survived']

model = svm.SVC(C=5000, gamma=0.01)
model.fit(x, y)

print np.mean(cross_validation.cross_val_score(model, x, y, cv=10, n_jobs=-1))
