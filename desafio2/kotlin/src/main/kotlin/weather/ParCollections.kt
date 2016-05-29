package weather

// borrowed from https://github.com/holgerbrandl/kutils/blob/master/src/main/kotlin/kutils/ParCollections.kt

import java.util.*
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

/**
 * Parallel collection mimicking  scala's .par().
 *
 * @author Holger Brandl
 */

/** A delegated tagging interface to allow for parallized extension functions */
class ParCol<T>(val it: Iterable<T>, val executorService: ExecutorService) : Iterable<T> by it


/** Convert a stream into a parallel collection. */
fun <T> Iterable<T>.par(numThreads: Int = Runtime.getRuntime().availableProcessors(),
                        executorService: ExecutorService = Executors.newFixedThreadPool(numThreads)): ParCol<T> {
    return ParCol(this, executorService);
}


/** De-parallelize a collection. Undos <code>par</code> */
fun <T> ParCol<T>.unpar(): Iterable<T> {
    return this.it;
}


fun <T, R> ParCol<T>.map(transform: (T) -> R): ParCol<R> {
    // note default size is just an inlined version of kotlin.collections.collectionSizeOrDefault
    val destination = ArrayList<R>(if (this is Collection<*>) this.size else 10)

    // http://stackoverflow.com/questions/3269445/executorservice-how-to-wait-for-all-tasks-to-finish
    // use futures here to allow for recycling of the executorservice
    val futures = this.asIterable().map { executorService.submit { destination.add(transform(it)) } }
    futures.map { it.get() } // this will block until all are done

    //    executorService.shutdown()
    //    executorService.awaitTermination(1, TimeUnit.DAYS)

    return ParCol(destination, executorService)
}


/** A parallelizable version of kotlin.collections.map
 * @param numThreads The number of parallel threads. Defaults to number of cores minus 2
 * @param exec The executor. By exposing this as a parameter, application can share an executor among different parallel
 *             mapping taks.
 */
fun <T, R> Iterable<T>.parmap(numThreads: Int = Runtime.getRuntime().availableProcessors(),
                              exec: ExecutorService = Executors.newFixedThreadPool(numThreads),
                              transform: (T) -> R): Iterable<R> {
    return this.par(executorService = exec).map(transform).unpar()
}


