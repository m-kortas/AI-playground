from tensorflow.keras.models import Model
from tensorflow.keras.layers import Flatten, Dense, Dropout
from tensorflow.keras.preprocessing.image import ImageDataGenerator
import efficientnet.tfkeras as efn 
from tensorflow.keras.optimizers import Adam
import os
import numpy as np

train_dir = 'train'
validation_dir = 'validation'

batch_size = 128
epochs = 15
IMG_HEIGHT = 150
IMG_WIDTH = 150

image_gen_train = ImageDataGenerator(
                    rescale=1./255,
                    rotation_range=45,
                    width_shift_range=.15,
                    height_shift_range=.15,
                    horizontal_flip=True,
                    zoom_range=0.5)
image_gen_val = ImageDataGenerator(rescale=1./255)
train_data_gen = image_gen_train.flow_from_directory(batch_size=batch_size,
                                                     directory=train_dir,
                                                     shuffle=True,
                                                     target_size=(IMG_HEIGHT, IMG_WIDTH),
                                                     class_mode='binary')
val_data_gen = image_gen_val.flow_from_directory(batch_size=batch_size,
                                                 directory=validation_dir,
                                                 target_size=(IMG_HEIGHT, IMG_WIDTH),
                                                 class_mode='binary')

net = efn.EfficientNetB7(include_top=False,
                        weights='imagenet',
                        input_tensor=None,
                        input_shape=(IMG_HEIGHT, IMG_WIDTH))  
x = net.output
x = Flatten()(x)
x = Dropout(0.5)(x)
output_layer = Dense(2, activation='softmax', name='softmax')(x)
net_final = Model(inputs=net.input, outputs=output_layer)   

net_final.compile(optimizer=Adam(),
                  loss='binary_crossentropy', metrics=['accuracy'])

net_final.fit_generator(
    train_data_gen,
 #   steps_per_epoch=total_train // batch_size,
    epochs=epochs,
    validation_data=val_data_gen,
#    validation_steps=total_val // batch_size
)
