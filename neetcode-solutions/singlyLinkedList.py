
class LinkedList:
    def __init__(self):
        self.value = None
        self.next = None

    
    def get(self, index: int) -> int:
        if index == 0 and self.value != None:
            return self.value
        elif self.next != None: 
            return self.next.get(index-1)
        else:
            return -1
        

    def insertHead(self, val: int) -> None:
        if self.value == None:
            self.value = val
            return
        newTail = LinkedList()
        newTail.value = self.value
        newTail.next = self.next
        self.value = val
        self.next = newTail

    def insertTail(self, val: int) -> None:
        if self.value == None:
            self.value = val
            return

        cur = self
        while cur.next != None: 
            cur = cur.next

        tail = LinkedList()
        tail.value = val
        cur.next = tail

    def remove(self, index: int) -> bool:
        if index == 0:
            if self.value == None: 
                return False
            if self.next != None:
                self.value = self.next.value
                self.next = self.next.next
                return True
            else:
                self.value = None
                self.next = None
                return True
        cur = self

        while index > 1 and cur.next != None: 
            cur = cur.next
            index -= 1

        if cur.next != None:
            cur.next = cur.next.next
            return True
        else:
            return False

    def getValues(self) -> list[int]:
        if self.value == None: 
            return []
        cur = self
        arr = [self.value]
        while cur.next != None: 
            cur = cur.next
            arr.append(cur.value)
        return arr

if __name__ == "__main__":
    l = LinkedList()
    print(l.insertHead(1))
    print(l.remove(2))
    print(l.remove(1))

