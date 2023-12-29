#include <iostream>

class DynamicArray {
public:
  explicit DynamicArray(int capacity) {
    arr_size = 0;
    arr_capacity = capacity;
    arr = new int[capacity];
  }

  ~DynamicArray() {
    delete[] arr;
  }

  int get(int i) const {
    return arr[i];
  }

  void set(int i, int n) {
    arr[i] = n;
  }

  void pushback(int n) {
    if (arr_size+1 > arr_capacity) {
      resize();
    }
    arr[arr_size] = n;
    arr_size++;
  }

  int popback() {
    arr_size--;
    return arr[arr_size];
  }

  void resize() {
    arr_capacity += arr_capacity;
    arr = static_cast<int*>(realloc(arr, sizeof(int) * arr_capacity * 2));
  }

  int getSize() const {
    return arr_size;
  }

  int getCapacity() const {
    return arr_capacity;
  }

private:
  int* arr;
  int arr_capacity;
  int arr_size;
};

int main() { 
  DynamicArray arr = DynamicArray(1);
  std::cout << arr.getSize() << std::endl;
  std::cout << arr.getCapacity() << std::endl;
}
