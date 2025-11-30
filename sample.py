"""
Sample Python file for hs-indent-fold demonstration.

This file contains various nested structures to showcase
the indent-based folding functionality.
"""


class DataProcessor:
    """A sample class demonstrating nested code blocks."""

    def __init__(self, name, config=None):
        self.name = name
        self.config = config or {}
        self.data = []

    def process(self, items):
        """Process a list of items with multiple nested levels."""
        results = []

        for item in items:
            if self.validate(item):
                transformed = self.transform(item)
                if transformed:
                    results.append(transformed)
                else:
                    self.handle_error(item, "Transform failed")
            else:
                self.handle_error(item, "Validation failed")

        return results

    def validate(self, item):
        """Validate an item with complex conditions."""
        if item is None:
            return False

        if isinstance(item, dict):
            required_keys = ["id", "value"]
            for key in required_keys:
                if key not in item:
                    return False
            return True

        if isinstance(item, str):
            return len(item) > 0

        return False

    def transform(self, item):
        """Transform an item based on its type."""
        if isinstance(item, dict):
            return {
                "id": item["id"],
                "value": item["value"] * 2,
                "processed": True,
            }
        elif isinstance(item, str):
            return item.upper()
        else:
            return None

    def handle_error(self, item, message):
        """Log errors for failed items."""
        print(f"Error processing {item}: {message}")


class AsyncHandler:
    """Demonstrates deeply nested async-like patterns."""

    def __init__(self):
        self.callbacks = []
        self.state = "idle"

    def register_callback(self, callback):
        """Register a callback function."""
        if callable(callback):
            self.callbacks.append(callback)

    def execute(self, data):
        """Execute all callbacks with nested error handling."""
        results = []

        for callback in self.callbacks:
            try:
                result = callback(data)
                if result is not None:
                    if isinstance(result, list):
                        for item in result:
                            results.append(item)
                    else:
                        results.append(result)
            except Exception as e:
                print(f"Callback error: {e}")

        return results


def fibonacci(n):
    """Calculate fibonacci with recursion and memoization pattern."""
    cache = {}

    def fib_inner(x):
        if x in cache:
            return cache[x]

        if x <= 1:
            return x

        result = fib_inner(x - 1) + fib_inner(x - 2)
        cache[x] = result
        return result

    return fib_inner(n)


def parse_config(config_string):
    """Parse configuration with multiple nested conditions."""
    lines = config_string.strip().split("\n")
    config = {}

    for line in lines:
        line = line.strip()

        if not line:
            continue

        if line.startswith("#"):
            continue

        if "=" in line:
            key, value = line.split("=", 1)
            key = key.strip()
            value = value.strip()

            if value.startswith("[") and value.endswith("]"):
                items = value[1:-1].split(",")
                config[key] = [item.strip() for item in items]
            elif value.isdigit():
                config[key] = int(value)
            elif value.lower() in ("true", "false"):
                config[key] = value.lower() == "true"
            else:
                config[key] = value

    return config


if __name__ == "__main__":
    processor = DataProcessor("demo")

    test_data = [
        {"id": 1, "value": 10},
        {"id": 2, "value": 20},
        "hello",
        None,
        {"id": 3},
    ]

    results = processor.process(test_data)
    print(f"Processed {len(results)} items")

    print(f"Fibonacci(10) = {fibonacci(10)}")
