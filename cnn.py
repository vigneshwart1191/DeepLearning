# -*- coding: utf-8 -*-
"""
Created on Tue Feb 27 15:53:49 2018

@author: Vigneshwar.Thiyagara
"""

#Doing image classification
#Cnn algorith used
#COnsist f three steps 1.convolution,pooling and flattening

from keras.models import Sequential
from keras.layers import Convolution2D
from keras.layers import MaxPooling2D
from keras.layers import Flatten
from keras.layers import Dense
import os
os.chdir('C:/Users/vigneshwar.thiyagara/Documents/DataScience/DeepLearning/Data')
#Intializing the cnn
classifier = Sequential()

#Add convolution layer
classifier.add(Convolution2D(32,3,3,input_shape=(64,64,3),activation= 'relu'))

#Add pooling layer
classifier.add(MaxPooling2D(pool_size= (2,2)))

#Flattening layer
classifier.add(Flatten())

#Full connection which has hidden layer and output layer
classifier.add(Dense(units = 128,activation='relu'))
classifier.add(Dense(units = 1,activation='sigmoid'))

#Compile the ANN
classifier.compile(optimizer= 'adam',loss='binary_crossentropy',metrics=['accuracy'])

#Image preprocessing using Image generator
#This piece of code taken from keras documentation
from keras.preprocessing.image import ImageDataGenerator

train_datagen = ImageDataGenerator(rescale=1./255,
                                   shear_range=0.2,
                                   zoom_range=0.2,
                                   horizontal_flip=True)

test_datagen = ImageDataGenerator(rescale=1./255)

training_set = train_datagen.flow_from_directory('dataset/training_set',
                                                    target_size=(64, 64),
                                                    batch_size=32,
                                                    class_mode='binary')

test_set = test_datagen.flow_from_directory('dataset/test_set',
                                                target_size=(64, 64),
                                                batch_size=32,
                                                class_mode='binary')

classifier.fit_generator(training_set,
                        steps_per_epoch=8000,
                        epochs=25,
                        validation_data=test_set,
                        nb_val_samples = 2000)