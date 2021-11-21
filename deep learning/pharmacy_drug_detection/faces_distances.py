import editdistance as editdistance
import numpy as np

TRAIN_LABELS_FILE = "faces/faces_is/train/labels.txt"


def read_labels(filename):
    with open(filename) as f:
        lines = f.read().splitlines()
    return lines


def main():
    labels = read_labels(TRAIN_LABELS_FILE)
    labels_inv = labels[::-1]

    distances = []
    for first, second in zip(labels, labels_inv):
        dist = editdistance.eval(first, second)
        distances.append(dist)

    avg_dist = np.mean(distances)
    print(avg_dist)


if __name__ == '__main__':
    main()
