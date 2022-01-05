package huffman

import "container/heap"

type Heap []Tree

func (h Heap) Len() int {
	return len(h)
}

func (h Heap) Less(i, j int) bool {
	return h[i].Freq() < h[j].Freq()
}

func (h Heap) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *Heap) Push(x interface{}) {
	*h = append(*h, x.(Tree))
}

func (h *Heap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

func NewHeap(size int) *Heap {
	ph := &Heap{}
	heap.Init(ph)
	return ph
}

func (h *Heap) Insert(elem Tree) {
	heap.Push(h, elem)
}

func (h *Heap) Extract() Tree {
	t := heap.Pop(h).(Tree)
	return t
}

func (h *Heap) Size() int {
	return h.Len()
}
