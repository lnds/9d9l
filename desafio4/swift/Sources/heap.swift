import Foundation

class Heap {
	
	var data: [Tree?]
	var last: Int

	init(size: Int) {
		data = [Tree?](repeating: nil, count: size)
		last = 0
	}

   func insert(elem: Tree) {
		last += 1
		data[last] = elem
		var j = last
		while j > 1 {
			if data[j]!.freq() < data[j/2]!.freq() {
				let temp = data[j]
				data[j] = data[j/2]
				data[j/2] = temp
			}
			j /= 2
		}
	}

	func extract() -> Tree? {
		if last == 0 {
			return nil
		} else {
			let min = data[1]
			data[1] = data[last]
			last -= 1
			var j = 1
			while 2*j <= last {
				var k = 2 * j
				if k+1 <= last && data[k+1]!.freq() < data[k]!.freq() {
					k += 1
				}
				if data[j]!.freq() < data[k]!.freq() {
					break
				}
				let temp = data[j]
				data[j] = data[k]
				data[k] = temp
				j = k 
			}
			return min
		}
	}

	func size() -> Int {
		return last
	}
}

