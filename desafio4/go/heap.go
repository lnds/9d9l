package main


type Heap struct {
	data []Tree
	last int
}

func NewHeap(size int) *Heap {
	p := new(Heap)
	p.data = make([]Tree, size)
	p.last = 0
	return p
}

func (h *Heap) Insert(elem Tree) {
	h.last++
	h.data[h.last] = elem
	j := h.last
	for j > 1 {
		if h.data[j].Freq() < h.data[j/2].Freq() {
			temp := h.data[j]
			h.data[j] = h.data[j/2]
			h.data[j/2] = temp
		}
		j /= 2
	}
}

func (h *Heap) Extract() Tree {
	if h.last == 0 {
		return nil
	} else {
		min := h.data[1]
		h.data[1] = h.data[h.last]
		h.last--
		for j := 1; 2*j <= h.last;  {
			k := 2*j
			if k+1 <= h.last && h.data[k+1].Freq() < h.data[k].Freq() {
				k++
			}
			if h.data[j].Freq() < h.data[k].Freq() {
				break
			}
			temp := h.data[j]
			h.data[j] = h.data[k]
			h.data[k] = temp
			j = k
		}
		return min
	}
}

func (h *Heap) Size() int {
	return h.last
}