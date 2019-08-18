// swift-tools-version:5.0
import PackageDescription

let package = Package(
        name: "ordenar_vector",
        products: [
                .executable(name: "ordenar_vector", targets: ["ordenar_vector"])
        ],
        dependencies: [
        ],
        targets: [
                .target(name: "ordenar_vector", dependencies: [], path: "./Sources")
        ]
)