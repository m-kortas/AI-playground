import os
import cv2
from keras import Sequential
from keras import Model
from keras.callbacks import ModelCheckpoint, EarlyStopping, TensorBoard
from keras.layers import Conv2D, MaxPooling2D, Dense, Flatten, np, BatchNormalization, Activation, regularizers, Dropout
from keras.utils import np_utils
from keras.optimizers import RMSprop, Adam
from keras.layers import Embedding, LSTM
from keras.applications import ResNet50, MobileNet


TRAIN_LABELS_FILE = "faces/faces_is/train/labels.txt"
VAL_LABELS_FILE = "faces/faces_is/val/labels.txt"
TEST_LABELS_FILE = "faces/faces_is/test/labels.txt"


def read_labels(filename, number):
    with open(filename) as f:
        lines = f.read().splitlines()[:number]
    return lines


def create_dataset(lines, dirname):
    images = []
    labels = []

    for line in lines:
        path, label = line.split()
        path = os.path.join(dirname, path)
        img = cv2.imread(path)
        img = cv2.resize(img, (224, 224), interpolation=cv2.INTER_AREA)
        images.append(img)

        label = int(label)
        label = np_utils.to_categorical(label, 52)
        labels.append(label)

    return np.array(images), np.array(labels)


def show_dataset(images, labels):
    for img, l in zip(images, labels):
        print(l)
        cv2.imshow("image", img)
        cv2.waitKey()


def create_model():
    model = MobileNet(input_shape=(224,224,3), include_top=False, pooling="avg", weights="imagenet")
    dense = Dense(52, activation="softmax")(model.output)
    model = Model(inputs = [model.input], outputs= [dense])
    opt = Adam()
    model.compile(loss='categorical_crossentropy', metrics=['categorical_accuracy'], optimizer=opt)
   # model.load_weights("weights2.hdf5")
    return model



def get_callbacks():
    callbacks = []
  #  mc = ModelCheckpoint("weights2.hdf5", monitor="val_categorical_accuracy", save_best_only=True, verbose=1)
    es = EarlyStopping(monitor="val_categorical_accuracy", patience=2, verbose=1)
    ts = TensorBoard()
  #  callbacks.append(mc)
    callbacks.append(ts)
    callbacks.append(es)

    return callbacks


def main():
    train_lines = read_labels(TRAIN_LABELS_FILE, 100)
    train_images, train_labels = create_dataset(train_lines, os.path.dirname(TRAIN_LABELS_FILE))

    val_lines = read_labels(VAL_LABELS_FILE, 50)
    val_images, val_labels = create_dataset(val_lines, os.path.dirname(VAL_LABELS_FILE))

    test_lines = read_labels(TEST_LABELS_FILE, 50)
    test_images, test_labels = create_dataset(test_lines, os.path.dirname(TEST_LABELS_FILE))

    callbacks = get_callbacks()

    model = create_model()
    model.summary()
   # model.get_weights()
    model.fit(train_images, train_labels, batch_size=10, epochs=100, validation_data=(val_images, val_labels), callbacks=callbacks)


   # model.evaluate(test_images, test_labels, batch_size=10)

   # print(model.evaluate(test_images, test_labels, batch_size=10))
   # print(model.metrics_names)


if __name__ == '__main__':
    main()
