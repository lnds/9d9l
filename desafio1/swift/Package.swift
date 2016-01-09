import PackageDescription

let package = Package(
	name: "toque-y-fama",
	dependencies: [
		.Package(url: "https://github.com/apple/example-package-fisheryates.git", majorVersion: 1)
	]
)