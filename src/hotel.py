import pandas as pd
import numpy as np

df = pd.read_csv('hotel_bookings_raw.csv')

df.head()
df.info()
df.describe()

# remove company column
df.drop(['company', 'arrival_date_year', 'arrival_date_day_of_month', ], axis=1, inplace=True)

# convert childern column to integer
df['children'] = df['children'].fillna(0)
df['children'] = df['children'].astype(int)

df['children'].value_counts()
# check the information of children = 10
df[df['children'] == 10]
# drop the row with children = 10
df = df[df['children'] != 10]

# check reserved_room_type and assigned_room_type columns
# if reserved_room_type is different from assigned_room_type, it is considered as reassigned
# create a new column called room_reassigned
# if reserved_room_type is equal to assigned_room_type, room_reassigned = 0
# if reserved_room_type is different from assigned_room_type, room_reassigned = 1
df['room_reassigned'] = 0
df.loc[df['reserved_room_type'] != df['assigned_room_type'], 'room_reassigned'] = 1
# drop reserved_room_type and assigned_room_type columns
df.drop(['reserved_room_type', 'assigned_room_type'], axis=1, inplace=True)

df['reservation_status'].value_counts()
# drop reservation_status column
df.drop('reservation_status', axis=1, inplace=True)



cat_cols = df.select_dtypes(include='object').columns
cat_cols
num_cols = df.select_dtypes(include=np.number).columns
num_cols

df['agent'].value_counts()
# check the missing values in agent column
# drop agent column
df.drop('agent', axis=1, inplace=True)

# drop reservation_status_date column
df.drop('reservation_status_date', axis=1, inplace=True)

# drop rows with missing values
df.dropna(inplace=True)
df.info()

# check the unique values in each categorical column
for col in cat_cols:
    print(f'{col}: {df[col].unique()}')


# drop country column as it has too many unique values
df_wocountry = df.drop('country', axis=1)

cat_cols = df_wocountry.select_dtypes(include='object').columns
# convert categorical columns to numerical with one-hot encoding
df_wocountry = pd.get_dummies(df_wocountry, columns=cat_cols, drop_first=True)
df_wocountry.head()

#################
# save the cleaned data
#df.to_csv('hotel_bookings_cleaned_encode.csv', index=False)
#################

# split the data into X and y
X = df_wocountry.drop('is_canceled', axis=1)
y = df_wocountry['is_canceled']

# split the data into training and testing sets
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# standardize the data
from sklearn.preprocessing import StandardScaler
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

# train a logistic regression model
from sklearn.linear_model import LogisticRegression
logreg = LogisticRegression(max_iter=1000)
logreg.fit(X_train_scaled, y_train)

# make predictions
y_pred = logreg.predict(X_test_scaled)

# evaluate the model
from sklearn.metrics import accuracy_score
accuracy = accuracy_score(y_test, y_pred)

print(f'Accuracy: {accuracy:.2f}')

# confusion matrix
from sklearn.metrics import confusion_matrix
conf_matrix = confusion_matrix(y_test, y_pred)
print(conf_matrix)

# plot confusion matrix
import matplotlib.pyplot as plt
import seaborn as sns
sns.heatmap(conf_matrix, annot=True, fmt='d')
plt.xlabel('Predicted')
plt.ylabel('Actual')
plt.show()


# classification report
from sklearn.metrics import classification_report
class_report = classification_report(y_test, y_pred)
print(class_report)

# ROC curve
from sklearn.metrics import roc_curve, roc_auc_score
y_pred_prob = logreg.predict_proba(X_test_scaled)[:, 1]
fpr, tpr, thresholds = roc_curve(y_test, y_pred_prob)
plt.plot([0, 1], [0, 1], 'k--')
plt.plot(fpr, tpr)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve')
plt.show()

# find the most important features
logreg_coef = logreg.coef_[0]
logreg_coef = pd.Series(logreg_coef, index=X.columns)
logreg_coef = logreg_coef.sort_values(ascending=False)
logreg_coef[:10].plot(kind='bar')
plt.show()

logreg_coef













#df_wocountry['previous_cancellations'].value_counts()
# count plot of 'previous_cancellations' and the target variable 'is_canceled'
#sns.countplot(data=df_wocountry, x='previous_cancellations', hue='is_canceled')
#plt.show()

#df.groupby(['previous_cancellations', 'is_canceled']).size()

# show the count of each category in 'deposit_type' and 'is_canceled'
#df.groupby(['deposit_type', 'is_canceled']).size()
# count plot group by the 'deposit_type' and 'is_canceled'
#plt.figure(figsize=(10, 6))
#sns.countplot(data=df, x='deposit_type', hue='is_canceled')
#plt.show()

#df['deposit_type'].value_counts()
#sns.countplot(data=df, x='deposit_type', hue='is_canceled')
#plt.show()