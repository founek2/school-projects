import argparse


class CustomAction(argparse.Action):
    """Custom action for argparse for preserving arguments order
     by adding attribute 'ordered_args' - array of (param, argument)"""

    def __init__(self,
                 *args, **kwargs):
        super(CustomAction, self).__init__(*args, **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        if 'ordered_args' not in namespace:
            setattr(namespace, 'ordered_args', [])

        if not values:
            values = []

        namespace.ordered_args.append((self.dest, values))
